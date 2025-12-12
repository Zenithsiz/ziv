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
		ffi::{OsStr, OsString},
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
	pub fn file_name(&self) -> Result<OsString, AppError> {
		// TODO: Avoid having to clone the file name
		let source = self.source();
		match source {
			EntrySource::Path(path) => path.file_name().context("Missing file name").map(OsStr::to_owned),
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

	/// Tries to gets the metadata
	fn try_metadata(&self, thread_pool: &PriorityThreadPool) -> Result<Option<EntryMetadata>, AppError> {
		#[cloned(this = self)]
		self.0.metadata.try_load(thread_pool, Priority::DEFAULT, move || {
			self::load_metadata(&this.source()).context("Unable to get metadata")
		})
	}

	/// Sets the metadata of this entry
	pub(super) fn set_metadata(&self, metadata: EntryMetadata) {
		self.0.metadata.set(metadata);
	}
}

/// Data
impl DirEntry {
	/// Gets the entry's data, blocking
	fn data_blocking(&self, egui_ctx: &egui::Context) -> Result<EntryData, AppError> {
		self.0
			.data
			.load(|| self::load_entry_data(self.source(), egui_ctx).context("Unable to load entry data"))
	}

	/// Returns this entry's data
	pub fn try_data(
		&self,
		thread_pool: &PriorityThreadPool,
		egui_ctx: &egui::Context,
	) -> Result<Option<EntryData>, AppError> {
		#[cloned(this = self, egui_ctx)]
		self.0.data.try_load(thread_pool, Priority::HIGH, move || {
			self::load_entry_data(this.source(), &egui_ctx).context("Unable to load entry data")
		})
	}

	/// Returns this entry's data
	pub fn data_if_exists(&self) -> Result<Option<EntryData>, AppError> {
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
}

/// Size
impl DirEntry {
	/// Gets the size, blocking
	fn size_blocking(&self) -> Result<u64, AppError> {
		let metadata = self.metadata_blocking()?;
		Ok(metadata.size)
	}

	/// Returns this image's file size
	pub fn try_size(&self, thread_pool: &PriorityThreadPool) -> Result<Option<u64>, AppError> {
		let Some(metadata) = self.try_metadata(thread_pool)? else {
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
			let data = this.data_blocking(&egui_ctx).context("Unable to load data")?;
			EntryThumbnail::new(&egui_ctx, &thumbnails_dir, &source, &data).context("Unable to create thumbnail")
		})
	}
}

/// Misc.
impl DirEntry {
	/// Compares two directory entries according to a sort order
	pub(super) fn cmp_with(&self, other: &Self, order: SortOrder) -> Result<Ordering, AppError> {
		let cmp = match order.kind {
			SortOrderKind::FileName => {
				let lhs = self.file_name().context("Unable to get file name")?;
				let rhs = other.file_name().context("Unable to get file name")?;
				natord::compare_iter(
					lhs.as_encoded_bytes().iter(),
					rhs.as_encoded_bytes().iter(),
					|&c| c.is_ascii_whitespace(),
					|&l, &r| l.cmp(r),
					|&c| c.is_ascii_digit().then(|| isize::from(c - b'0')),
				)
			},
			SortOrderKind::ModificationDate => {
				let lhs = self.modified_date_blocking()?;
				let rhs = other.modified_date_blocking()?;

				SystemTime::cmp(&lhs, &rhs)
			},
			SortOrderKind::Size => {
				let lhs = self.size_blocking()?;
				let rhs = other.size_blocking()?;

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

/// Entry source
#[derive(Clone, Debug)]
pub enum EntrySource {
	/// Filesystem path
	Path(Arc<Path>),
}

/// Entry metadata
#[derive(Clone, Debug)]
pub struct EntryMetadata {
	modified_time: SystemTime,
	size:          u64,
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
	let metadata = match source {
		EntrySource::Path(path) => fs::metadata(path).context("Unable to get metadata")?,
	};

	EntryMetadata::from_file(&metadata)
}

fn load_entry_data(source: EntrySource, egui_ctx: &egui::Context) -> Result<EntryData, AppError> {
	let EntrySource::Path(path) = source;

	// Test against video file formats before we need to read the file.
	const COMMON_VIDEO_FORMATS: &[&str] = &["gif", "mkv", "mp4", "mov", "avi", "webm"];
	if let Some(ext) = path.extension().and_then(OsStr::to_str) &&
		COMMON_VIDEO_FORMATS.contains(&ext)
	{
		let video = EntryVideo::new(egui_ctx, path).context("Unable to create video")?;
		return Ok(EntryData::Video(video));
	}

	// If we got a format just from the path, return it
	// TODO: Should we trust the extension?
	if let Some(ext) = path.extension() &&
		let Some(format) = ImageFormat::from_extension(ext)
	{
		let image = EntryImage::new(path, format);
		return Ok(EntryData::Image(image));
	}

	// If it's a directory it's non-media
	if fs::metadata(&path).context("Unable to get file metadata")?.is_dir() {
		return Ok(EntryData::Other);
	}

	// Otherwise, try to guess it by opening it with `image`
	let reader = ::image::ImageReader::open(&path)
		.context("Unable to create image reader")?
		.with_guessed_format()
		.context("Unable to read file")?;
	if let Some(format) = reader.format() {
		let image = EntryImage::new(path, format);
		return Ok(EntryData::Image(image));
	}

	// Then finally, try to guess it with `ffmpeg`.
	// Note: This detects quite a few thing we don't want to open,
	//       such as text files, so we ignore part of the output.
	// TODO: Currently we detect `svg`s as a video format (using
	//       `svg_pipe`). However, `image` doesn't support `svg`s,
	//       so for now we allow this.
	const DISALLOWED_VIDEO_FORMATS: &[&str] = &["lrc", "tty"];
	if let Ok(input) = ffmpeg_next::format::input(&path) &&
		!DISALLOWED_VIDEO_FORMATS.contains(&input.format().name())
	{
		let video = EntryVideo::new(egui_ctx, path).context("Unable to create video")?;
		return Ok(EntryData::Video(video));
	}

	Ok(EntryData::Other)
}
