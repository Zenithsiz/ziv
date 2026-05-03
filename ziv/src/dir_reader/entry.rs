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
		path::{Path, PathBuf},
		sync::Arc,
		time::SystemTime,
	},
	zutil_cloned::cloned,
};

#[derive(Debug)]
struct Inner {
	source: Mutex<EntrySource>,

	// TODO: Should we keep these here?
	//       Storing these means that when changing sort order
	//       we don't need to re-read the files, but it also means
	//       storing more data that might be unnecessary.
	metadata: Loadable<EntryMetadata>,
	data:     EntryData,
}

#[derive(Clone, Debug)]
pub struct DirEntry(Arc<Inner>);

impl DirEntry {
	/// Creates a new directory entry
	pub(super) fn new(source: EntrySource, file_type: EntryFileType) -> Self {
		let data = EntryData::guess(&source, file_type);

		Self(Arc::new(Inner {
			source: Mutex::new(source),
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
	/// Gets the entry's data
	pub fn data(&self) -> &EntryData {
		&self.0.data
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
		let resolution = match self.data() {
			EntryData::DisplayGuess(_) | EntryData::Unknown => {
				let Some(display) = loaded_displays.get_if_loaded(self)? else {
					return Ok(None);
				};
				match display {
					EntryDisplay::Image(image) => image.resolution_if_loaded()?,
					EntryDisplay::Video(video) => video.resolution_if_loaded()?,
				}
			},
			EntryData::NonMedia => Some(EntryResolution { width: 0, height: 0 }),
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
#[derive(PartialEq, Eq, Clone, Debug)]
pub enum EntryData {
	/// Display-able data
	DisplayGuess(EntryDisplayGuess),

	/// Unknown
	Unknown,

	/// Non-media
	NonMedia,
}

impl EntryData {
	/// Guesses entry data from an entry
	// TODO: Should we be trusting the extension in here?
	fn guess(source: &EntrySource, file_type: EntryFileType) -> Self {
		let file_name = match source {
			EntrySource::Path(path) => match path.file_name() {
				Some(file_name) => Path::new(file_name),
				// If it has no file name, it's definitely non-media
				None => return Self::NonMedia,
			},
			EntrySource::Zip(zip) => zip.file_name(),
		};

		// If it's a directory it's non-media
		if file_type.is_dir() {
			return Self::NonMedia;
		}

		// If it has no extension (or it's non-utf8), we consider it non-media
		let Some(ext_str) = file_name.extension().and_then(OsStr::to_str) else {
			return Self::NonMedia;
		};

		// Test against common non-media extensions
		// TODO: This list needs to be much bigger
		const COMMON_NON_MEDIA_FORMATS: &[&str] =
			&["zip", "cbz", "pdf", "xlsx", "7z", "jar", "gz", "txt", "csv", "rtf"];
		if COMMON_NON_MEDIA_FORMATS.contains(&ext_str) {
			return Self::NonMedia;
		}

		// Test against video file formats
		const COMMON_VIDEO_FORMATS: &[&str] = &["gif", "mkv", "mp4", "mov", "avi", "webm"];
		if COMMON_VIDEO_FORMATS.contains(&ext_str) {
			return Self::DisplayGuess(EntryDisplayGuess::Video);
		}

		// Finally just test against image formats
		if let Some(format) = ImageFormat::from_extension(ext_str) {
			return Self::DisplayGuess(EntryDisplayGuess::Image { format });
		}

		Self::Unknown
	}
}

/// Entry display guess
#[derive(PartialEq, Eq, Clone, Debug)]
pub enum EntryDisplayGuess {
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
