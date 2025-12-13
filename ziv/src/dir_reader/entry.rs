//! Directory entry

// Modules
pub mod image;
pub mod thumbnail;
pub mod video;

// Exports
pub use self::{image::EntryImage, thumbnail::EntryThumbnail, video::EntryVideo};

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
		sync::Arc,
		time::SystemTime,
	},
	zip::{ZipArchive, read::ZipFile},
	zutil_cloned::cloned,
};

#[derive(Debug)]
struct Inner {
	source: Mutex<EntrySource>,

	metadata:  Loadable<EntryMetadata>,
	data:      Loadable<EntryData>,
	thumbnail: Loadable<EntryThumbnail>,
}

#[derive(Clone, Debug)]
pub struct DirEntry(Arc<Inner>);

impl DirEntry {
	/// Creates a new directory entry
	pub(super) fn new(source: EntrySource) -> Self {
		Self(Arc::new(Inner {
			source:    Mutex::new(source),
			metadata:  Loadable::new(),
			data:      Loadable::new(),
			thumbnail: Loadable::new(),
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
			EntrySource::Zip(zip) => Ok(zip.file_name.clone()),
		}
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
	pub(super) fn cmp_with(&self, other: &Self, order: SortOrder) -> Result<Ordering, AppError> {
		let cmp = match order.kind {
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
		};

		let order = match order.reverse {
			true => cmp.reverse(),
			false => cmp,
		};

		Ok(order)
	}

	/// Returns if this entry is loaded for `order`
	pub(super) fn is_loaded_for_order(&self, order: SortOrder) -> bool {
		match order.kind {
			SortOrderKind::FileName => true,
			SortOrderKind::ModificationDate | SortOrderKind::Size => self.0.metadata.is_loaded(),
		}
	}

	/// Loads the necessary fields for `order`
	pub(super) fn load_for_order(&self, order: SortOrder) -> Result<(), AppError> {
		match order.kind {
			SortOrderKind::FileName => _ = self.file_name().context("Unable to load file name")?,
			SortOrderKind::ModificationDate => _ = self.modified_date_blocking()?,
			SortOrderKind::Size => _ = self.size_blocking()?,
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

/// Entry source from a zip file
#[derive(Clone, Debug)]
pub struct EntrySourceZip {
	pub path:      Arc<Path>,
	pub file_name: PathBuf,
	pub metadata:  EntryMetadata,
	pub idx:       usize,
	// TODO: Should we move this elsewhere to avoid every entry storing it?
	pub archive:   Arc<Mutex<ZipArchive<fs::File>>>,
}

impl EntrySourceZip {
	/// Accesses this file.
	pub fn try_with_file<O>(
		&self,
		f: impl FnOnce(ZipFile<'_, fs::File>) -> Result<O, AppError>,
	) -> Result<O, AppError> {
		let mut archive = self.archive.lock();
		let file = archive.by_index(self.idx).context("Unable to get zip file")?;
		f(file)
	}

	/// Reads the contents of this entry
	pub fn contents(&self) -> Result<Vec<u8>, AppError> {
		self.try_with_file(|mut file| {
			let mut contents = Vec::with_capacity(file.size() as usize);
			file.read_to_end(&mut contents).context("Unable to read zip file")?;

			Ok(contents)
		})
	}

	/// Returns if the entry is a directory
	pub fn is_dir(&self) -> Result<bool, AppError> {
		self.try_with_file(|file| Ok(file.is_dir()))
	}
}

/// Entry source
#[derive(Clone, Debug)]
pub enum EntrySource {
	/// Filesystem path
	Path(Arc<Path>),

	/// Zip file
	Zip(Arc<EntrySourceZip>),
}

impl EntrySource {
	/// Returns a name for this entry
	pub fn name(&self) -> String {
		match self {
			Self::Path(path) => path.display().to_string(),
			Self::Zip(zip) => zip.path.join(&zip.file_name).display().to_string(),
		}
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

fn load_metadata(source: &EntrySource) -> Result<EntryMetadata, AppError> {
	match source {
		EntrySource::Path(path) => {
			let metadata = fs::metadata(path).context("Unable to get metadata")?;
			EntryMetadata::from_file(&metadata)
		},
		EntrySource::Zip(zip) => Ok(zip.metadata.clone()),
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
	let is_dir = match &source {
		EntrySource::Path(path) => fs::metadata(path).context("Unable to get file metadata")?.is_dir(),
		// TODO: Check for directories inside of zip files
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
