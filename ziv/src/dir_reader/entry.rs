//! Directory entry

// Modules
pub mod image;
pub mod key;
pub mod loaded_displays;
pub mod loaded_thumbnails;
pub mod source;
pub mod thumbnail;
pub mod video;

// Exports
pub use self::{
	image::EntryImage,
	loaded_displays::EntryLoadedDisplays,
	loaded_thumbnails::EntryLoadedThumbnails,
	source::{EntrySource, EntrySourceZip},
	thumbnail::EntryThumbnail,
	video::EntryVideo,
};

// Imports
use {
	self::key::Key,
	super::{SortOrder, SortOrderKind},
	crate::util::{AppError, Loadable, PriorityThreadPool, priority_thread_pool::Priority},
	::image::ImageFormat,
	app_error::Context,
	core::hash::Hash,
	parking_lot::Mutex,
	std::{
		ffi::OsStr,
		fs,
		io::{self, Read},
		path::PathBuf,
		sync::Arc,
		time::SystemTime,
	},
	zutil_cloned::cloned,
};

#[derive(Debug)]
struct Inner {
	source:    Mutex<EntrySource>,
	file_type: EntryFileType,

	// TODO: Should we keep these here?
	//       Storing these means that when changing sort order
	//       we don't need to re-read the files, but it also means
	//       storing more data that might be unnecessary.
	metadata: Loadable<EntryMetadata>,
	data:     Loadable<EntryData>,
}

#[derive(Clone, Debug)]
pub struct DirEntry(Arc<Inner>);

impl DirEntry {
	/// Creates a new directory entry
	pub(super) fn new(source: EntrySource, file_type: EntryFileType) -> Self {
		let data = match file_type.is_dir() {
			true => Loadable::loaded(EntryData::Other),
			false => Loadable::new(),
		};

		Self(Arc::new(Inner {
			source: Mutex::new(source),
			file_type,
			metadata: Loadable::new(),
			data,
		}))
	}
}

/// Path
impl DirEntry {
	/// Returns this entry's source
	pub fn source(&self) -> EntrySource {
		self.0.source.lock().clone()
	}

	/// Renames this entry
	pub fn rename(&self, path: PathBuf) {
		*self.0.source.lock() = EntrySource::Path(Arc::from(path));
	}
}

/// File name
impl DirEntry {
	/// Returns this entry's file name
	pub fn file_name(&self) -> Result<PathBuf, AppError> {
		// TODO: Avoid having to clone the file name
		let source = self.source();
		match source {
			EntrySource::Path(path) => path.file_name().context("Missing file name").map(PathBuf::from),
			EntrySource::Zip(zip) => Ok(zip.file_name().to_owned()),
		}
	}
}

/// File type
impl DirEntry {
	/// Returns this entry's file type
	pub fn file_type(&self) -> EntryFileType {
		self.0.file_type
	}
}

/// Metadata
impl DirEntry {
	/// Gets the metadata, blocking
	fn metadata_blocking(&self) -> Result<EntryMetadata, AppError> {
		self.0
			.metadata
			.load(|| self::load_metadata(&self.source()).context("Unable to get metadata"))
	}

	/// Gets the metadata, loading it
	fn metadata_load(&self, thread_pool: &PriorityThreadPool) -> Result<Option<EntryMetadata>, AppError> {
		#[cloned(this = self)]
		self.0.metadata.try_load(thread_pool, Priority::DEFAULT, move || {
			self::load_metadata(&this.source()).context("Unable to get metadata")
		})
	}

	/// Gets the metadata, if loaded
	fn metadata_if_loaded(&self) -> Result<Option<EntryMetadata>, AppError> {
		self.0.metadata.try_get()
	}

	/// Sets the metadata of this entry
	pub(super) fn set_metadata(&self, metadata: EntryMetadata) {
		self.0.metadata.set(metadata);
	}
}

/// Data
impl DirEntry {
	/// Gets the entry's data, blocking
	fn data_blocking(&self) -> Result<EntryData, AppError> {
		self.0
			.data
			.load(|| self::load_entry_data(self).context("Unable to load entry data"))
	}

	/// Gets the entry's data, loading it
	pub fn data_load(&self, thread_pool: &PriorityThreadPool) -> Result<Option<EntryData>, AppError> {
		#[cloned(this = self)]
		self.0.data.try_load(thread_pool, Priority::HIGH, move || {
			self::load_entry_data(&this).context("Unable to load entry data")
		})
	}

	/// Gets the entry's data, if loaded
	pub fn data_if_loaded(&self) -> Result<Option<EntryData>, AppError> {
		self.0.data.try_get()
	}
}

/// Modified date
impl DirEntry {
	/// Gets the modified date, blocking
	fn modified_date_blocking(&self) -> Result<SystemTime, AppError> {
		let metadata = self.metadata_blocking()?;
		Ok(metadata.modified_time)
	}

	/// Gets the modified date, if loaded
	pub fn modified_date_if_loaded(&self) -> Result<Option<SystemTime>, AppError> {
		let Some(metadata) = self.metadata_if_loaded()? else {
			return Ok(None);
		};

		Ok(Some(metadata.modified_time))
	}
}

/// Size
impl DirEntry {
	/// Gets the size, blocking
	fn size_blocking(&self) -> Result<u64, AppError> {
		let metadata = self.metadata_blocking()?;
		Ok(metadata.size)
	}

	/// Gets the size, loading it
	pub fn size_load(&self, thread_pool: &PriorityThreadPool) -> Result<Option<u64>, AppError> {
		let Some(metadata) = self.metadata_load(thread_pool)? else {
			return Ok(None);
		};

		Ok(Some(metadata.size))
	}

	/// Gets the size, if loaded
	pub fn size_if_loaded(&self) -> Result<Option<u64>, AppError> {
		let Some(metadata) = self.metadata_if_loaded()? else {
			return Ok(None);
		};

		Ok(Some(metadata.size))
	}
}

/// Resolution
impl DirEntry {
	/// Gets the resolution, if loaded
	pub fn resolution_if_loaded(
		&self,
		loaded_displays: &EntryLoadedDisplays,
	) -> Result<Option<EntryResolution>, AppError> {
		let Some(data) = self.data_if_loaded()? else {
			return Ok(None);
		};

		let resolution = match data {
			EntryData::Display(_) => {
				let Some(display) = loaded_displays.get_if_loaded(self)? else {
					return Ok(None);
				};
				match display {
					EntryDisplay::Image(image) => image.resolution_if_loaded()?,
					EntryDisplay::Video(video) => video.resolution_if_loaded()?,
				}
			},
			// TODO: Is a resolution of 0 fine for these?
			EntryData::Other => Some(EntryResolution { width: 0, height: 0 }),
		};

		Ok(resolution)
	}
}

/// Misc.
impl DirEntry {
	/// Returns if this entry is loaded for `order`
	pub(super) fn is_loaded_for_order(
		&self,
		order: SortOrder,
		loaded_displays: &EntryLoadedDisplays,
	) -> Result<bool, AppError> {
		match order.kind {
			SortOrderKind::FileName => key::FileName::is_loaded(self, loaded_displays),
			SortOrderKind::ModificationDate => key::ModificationDate::is_loaded(self, loaded_displays),
			SortOrderKind::Size => key::Size::is_loaded(self, loaded_displays),
			SortOrderKind::ResolutionWidth => key::ResolutionWidth::is_loaded(self, loaded_displays),
			SortOrderKind::ResolutionHeight => key::ResolutionHeight::is_loaded(self, loaded_displays),
			SortOrderKind::Random => key::Random::is_loaded(self, loaded_displays),
		}
	}

	/// Loads the necessary fields for `order`
	pub(super) fn load_for_order(
		&self,
		order: SortOrder,
		loaded_displays: &EntryLoadedDisplays,
	) -> Result<(), AppError> {
		match order.kind {
			SortOrderKind::FileName => key::FileName::load(self, loaded_displays),
			SortOrderKind::ModificationDate => key::ModificationDate::load(self, loaded_displays),
			SortOrderKind::Size => key::Size::load(self, loaded_displays),
			SortOrderKind::ResolutionWidth => key::ResolutionWidth::load(self, loaded_displays),
			SortOrderKind::ResolutionHeight => key::ResolutionHeight::load(self, loaded_displays),
			SortOrderKind::Random => key::Random::load(self, loaded_displays),
		}
	}
}

impl PartialEq for DirEntry {
	fn eq(&self, other: &Self) -> bool {
		Arc::ptr_eq(&self.0, &other.0)
	}
}

impl Eq for DirEntry {}

impl Hash for DirEntry {
	fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
		Arc::as_ptr(&self.0).hash(state);
	}
}

/// Entry metadata
#[derive(Clone, Debug)]
pub struct EntryMetadata {
	pub modified_time: SystemTime,
	pub size:          u64,
}

impl EntryMetadata {
	/// Creates entry metadata from file metadata
	pub fn from_file(metadata: &fs::Metadata) -> Result<Self, AppError> {
		Ok(Self {
			modified_time: metadata.modified().context("Unable to get modified time")?,
			size:          metadata.len(),
		})
	}
}

/// Entry data
#[derive(Clone, Debug)]
pub enum EntryData {
	/// Display-able data
	Display(EntryDisplayKind),

	/// Other
	Other,
}

/// Entry display kind
#[derive(Clone, Debug)]
pub enum EntryDisplayKind {
	Image { format: ImageFormat },
	Video,
}

/// Entry Display
#[derive(Clone, Debug)]
pub enum EntryDisplay {
	Image(EntryImage),
	Video(EntryVideo),
}

/// Entry file type
#[derive(Clone, Copy, Debug)]
pub enum EntryFileType {
	File,
	Symlink,
	Directory,
}
impl EntryFileType {
	/// Creates a file type from a filesystem file type
	pub fn from_file(file_type: fs::FileType) -> Self {
		match file_type {
			_ if file_type.is_dir() => Self::Directory,
			_ if file_type.is_symlink() => Self::Symlink,
			_ => Self::File,
		}
	}

	/// Returns if this file type is a directory
	pub const fn is_dir(self) -> bool {
		matches!(self, Self::Directory)
	}
}

fn load_metadata(source: &EntrySource) -> Result<EntryMetadata, AppError> {
	match source {
		EntrySource::Path(path) => {
			let metadata = fs::metadata(path).context("Unable to get metadata")?;
			EntryMetadata::from_file(&metadata)
		},
		EntrySource::Zip(zip) => Ok(zip.metadata().clone()),
	}
}

/// Entry resolution
#[derive(Clone, Debug)]
pub struct EntryResolution {
	// TODO: These should probably be `u32`s.
	pub width:  usize,
	pub height: usize,
}

// TODO: De-duplicate this with `loaded_displays::load`.
fn load_entry_data(entry: &DirEntry) -> Result<EntryData, AppError> {
	let source = entry.source();
	let file_name = entry.file_name().context("Entry had no file name")?;

	// If it's a directory it's non-media
	if entry.file_type().is_dir() {
		return Ok(EntryData::Other);
	}

	// Test against common zip archives
	const COMMON_ZIP_FORMATS: &[&str] = &["zip", "cbz"];
	if let Some(ext) = file_name.extension().and_then(OsStr::to_str) &&
		COMMON_ZIP_FORMATS.contains(&ext)
	{
		return Ok(EntryData::Other);
	}

	// Test against video file formats before we need to read the file.
	const COMMON_VIDEO_FORMATS: &[&str] = &["gif", "mkv", "mp4", "mov", "avi", "webm"];
	if let Some(ext) = file_name.extension().and_then(OsStr::to_str) &&
		COMMON_VIDEO_FORMATS.contains(&ext)
	{
		return Ok(EntryData::Display(EntryDisplayKind::Video));
	}

	// If we got a format just from the path, return it
	// TODO: Should we trust the extension?
	if let Some(ext) = file_name.extension() &&
		let Some(format) = ImageFormat::from_extension(ext)
	{
		return Ok(EntryData::Display(EntryDisplayKind::Image { format }));
	}

	// If it's a directory it's non-media
	// Note: This check involves touching the filesystem, so we only
	//       do it after checking everything we can without IO
	let is_dir = match &source {
		EntrySource::Path(path) => fs::metadata(path).context("Unable to get file metadata")?.is_dir(),
		EntrySource::Zip(zip) => zip.is_dir().context("Unable to check if zip entry was a directory")?,
	};
	if is_dir {
		return Ok(EntryData::Other);
	}

	// Otherwise, try to guess it by opening it with `image`
	let format = match &source {
		EntrySource::Path(path) => ::image::ImageReader::open(path)
			.context("Unable to create image reader")?
			.with_guessed_format()
			.context("Unable to read file")?
			.format(),

		// TODO: `with_guessed_format` only uses the first 16 bytes, so we just read that
		EntrySource::Zip(zip) => zip
			.try_with_file(|mut file| {
				let mut contents = [0; 16];
				file.read_exact(&mut contents).context("Unable to read contents")?;
				let format = ::image::ImageReader::new(io::Cursor::new(contents))
					.with_guessed_format()
					.context("Unable to read file")?
					.format();

				Ok(format)
			})
			.context("Unable to guess zip entry format")?,
	};
	if let Some(format) = format {
		return Ok(EntryData::Display(EntryDisplayKind::Image { format }));
	}

	// Then finally, try to guess it with `ffmpeg`.
	// Note: This detects quite a few thing we don't want to open,
	//       such as text files, so we ignore part of the output.
	// TODO: Currently we detect `svg`s as a video format (using
	//       `svg_pipe`). However, `image` doesn't support `svg`s,
	//       so for now we allow this.
	// TODO: Support videos inside of other sources
	if let EntrySource::Path(path) = source {
		const DISALLOWED_VIDEO_FORMATS: &[&str] = &["lrc", "tty", "mjpeg"];
		if let Ok(input) = ffmpeg_next::format::input(&path) &&
			!DISALLOWED_VIDEO_FORMATS.contains(&input.format().name())
		{
			return Ok(EntryData::Display(EntryDisplayKind::Video));
		}
	}

	Ok(EntryData::Other)
}
