//! Directory entry

// Modules
pub mod image;
pub mod source;
pub mod thumbnail;
pub mod video;

// Exports
pub use self::{
	image::EntryImage,
	source::{EntrySource, EntrySourceZip},
	thumbnail::EntryThumbnail,
	video::EntryVideo,
};

// Imports
use {
	super::{SortOrder, SortOrderKind},
	crate::util::{AppError, Loadable, PriorityThreadPool, priority_thread_pool::Priority},
	::image::ImageFormat,
	app_error::Context,
	core::{cmp::Ordering, hash::Hash},
	parking_lot::Mutex,
	std::{
		ffi::OsStr,
		fs,
		io::{self, Read},
		path::{Path, PathBuf},
		random,
		sync::Arc,
		time::SystemTime,
	},
	zutil_cloned::cloned,
};

#[derive(Debug)]
struct Inner {
	source:    Mutex<EntrySource>,
	file_type: EntryFileType,

	metadata:  Loadable<EntryMetadata>,
	data:      Loadable<EntryData>,
	thumbnail: Loadable<EntryThumbnail>,
	random:    Mutex<Option<u64>>,
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
			thumbnail: Loadable::new(),
			random: Mutex::new(None),
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
	fn modified_date_if_loaded(&self) -> Result<Option<SystemTime>, AppError> {
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

/// Thumbnail
impl DirEntry {
	/// Returns this image's thumbnail
	pub fn thumbnail(
		&self,
		thread_pool: &PriorityThreadPool,
		egui_ctx: &egui::Context,
		thumbnails_dir: &Arc<Path>,
	) -> Result<Option<EntryThumbnail>, AppError> {
		#[cloned(this = self, egui_ctx, thumbnails_dir)]
		self.0.thumbnail.try_load(thread_pool, Priority::LOW, move || {
			let source = this.source();
			let data = this.data_blocking().context("Unable to load data")?;
			EntryThumbnail::new(&egui_ctx, &thumbnails_dir, &source, &data).context("Unable to create thumbnail")
		})
	}
}

/// Misc.
impl DirEntry {
	/// Compares two directory entries according to a sort order.
	///
	/// # Equality
	/// If the two are equal according to the sort order, they will
	/// next be sorted by source.
	///
	/// # Directories
	/// Directories will always be sorted *before* any other entries
	pub(super) fn cmp_with(&self, other: &Self, order: SortOrder) -> Result<Ordering, AppError> {
		let cmp = self.cmp_with_inner(other, order.kind)?;
		let cmp = match cmp {
			Ordering::Equal => self.source().cmp(&other.source()),
			_ => cmp,
		};

		let order = match order.reverse {
			true => cmp.reverse(),
			false => cmp,
		};

		Ok(order)
	}

	/// Helper for [`Self::cmp_with`] that does not check for item equality
	/// nor for reverse orders
	fn cmp_with_inner(&self, other: &Self, order_kind: SortOrderKind) -> Result<Ordering, AppError> {
		// Sort directories before files
		match (self.file_type(), other.file_type()) {
			(EntryFileType::Directory, EntryFileType::File) => return Ok(Ordering::Less),
			(EntryFileType::File, EntryFileType::Directory) => return Ok(Ordering::Greater),
			_ => (),
		}

		let cmp = match order_kind {
			SortOrderKind::FileName => {
				let lhs = self.file_name().context("Unable to get file name")?;
				let rhs = other.file_name().context("Unable to get file name")?;
				natord::compare_iter(
					lhs.as_os_str().as_encoded_bytes().iter(),
					rhs.as_os_str().as_encoded_bytes().iter(),
					|&c| c.is_ascii_whitespace(),
					|&l, &r| l.cmp(r),
					|&c| c.is_ascii_digit().then(|| isize::from(c - b'0')),
				)
			},
			SortOrderKind::ModificationDate => {
				let lhs = self.modified_date_if_loaded()?.context("Missing modified date")?;
				let rhs = other.modified_date_if_loaded()?.context("Missing modified date")?;

				SystemTime::cmp(&lhs, &rhs)
			},
			SortOrderKind::Size => {
				let lhs = self.size_if_loaded()?.context("Missing size")?;
				let rhs = other.size_if_loaded()?.context("Missing size")?;

				u64::cmp(&lhs, &rhs)
			},
			SortOrderKind::ResolutionWidth | SortOrderKind::ResolutionHeight => {
				fn resolution(entry: &DirEntry) -> Result<EntryResolution, AppError> {
					let data = entry.data_if_loaded()?.context("Missing data")?;
					let resolution = match data {
						EntryData::Image(image) => image.resolution_if_loaded()?.context("Missing resolution")?,
						EntryData::Video(video) => video.resolution_if_loaded()?.context("Missing resolution")?,
						// TODO: Is a resolution of 0 fine for these?
						EntryData::Other => EntryResolution { width: 0, height: 0 },
					};

					Ok(resolution)
				}

				let lhs = resolution(self)?;
				let rhs = resolution(other)?;
				match order_kind {
					SortOrderKind::ResolutionWidth => usize::cmp(&lhs.width, &rhs.width),
					SortOrderKind::ResolutionHeight => usize::cmp(&lhs.height, &rhs.height),
					_ => unreachable!(),
				}
			},
			SortOrderKind::Random => {
				let lhs = self.0.random.lock().context("Missing random")?;
				let rhs = other.0.random.lock().context("Missing random")?;

				u64::cmp(&lhs, &rhs)
			},
		};

		Ok(cmp)
	}

	/// Returns if this entry is loaded for `order`
	pub(super) fn is_loaded_for_order(&self, order: SortOrder) -> Result<bool, AppError> {
		let is_loaded = match order.kind {
			SortOrderKind::FileName => true,
			SortOrderKind::ModificationDate | SortOrderKind::Size => self.0.metadata.is_loaded(),
			SortOrderKind::ResolutionWidth | SortOrderKind::ResolutionHeight => {
				let Some(data) = self.0.data.try_get()? else {
					return Ok(false);
				};

				match data {
					EntryData::Image(image) => image.resolution_is_loaded(),
					EntryData::Video(video) => video.resolution_is_loaded(),
					EntryData::Other => true,
				}
			},
			SortOrderKind::Random => self.0.random.lock().is_some(),
		};

		Ok(is_loaded)
	}

	/// Loads the necessary fields for `order`
	pub(super) fn load_for_order(&self, order: SortOrder) -> Result<(), AppError> {
		match order.kind {
			SortOrderKind::FileName => _ = self.file_name().context("Unable to load file name")?,
			SortOrderKind::ModificationDate => _ = self.modified_date_blocking()?,
			SortOrderKind::Size => _ = self.size_blocking()?,
			SortOrderKind::ResolutionWidth | SortOrderKind::ResolutionHeight => match self.data_blocking()? {
				EntryData::Image(image) => _ = image.resolution_blocking()?,
				EntryData::Video(video) => _ = video.resolution_blocking()?,
				EntryData::Other => (),
			},
			SortOrderKind::Random => *self.0.random.lock() = Some(random::random(..)),
		}

		Ok(())
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
	Image(EntryImage),
	Video(EntryVideo),
	Other,
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
		// TODO: Support videos inside of other sources
		let EntrySource::Path(path) = source else {
			app_error::bail!("Videos are only supported by path")
		};

		let video = EntryVideo::new(path).context("Unable to create video")?;
		return Ok(EntryData::Video(video));
	}

	// If we got a format just from the path, return it
	// TODO: Should we trust the extension?
	if let Some(ext) = file_name.extension() &&
		let Some(format) = ImageFormat::from_extension(ext)
	{
		let image = EntryImage::new(source, format);
		return Ok(EntryData::Image(image));
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
		let image = EntryImage::new(source, format);
		return Ok(EntryData::Image(image));
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
			let video = EntryVideo::new(path).context("Unable to create video")?;
			return Ok(EntryData::Video(video));
		}
	}

	Ok(EntryData::Other)
}
