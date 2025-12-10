//! Directory entry

// TODO: All context should be inside of `.try_load`

// Modules
pub mod image;
pub mod video;

// Exports
pub use self::{image::EntryImage, video::EntryVideo};

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
		fs::{self, Metadata},
		path::{Path, PathBuf},
		sync::Arc,
		time::SystemTime,
	},
	zutil_cloned::cloned,
};

#[derive(Debug)]
struct Inner {
	path: Mutex<Arc<Path>>,

	metadata:          Loadable<Arc<Metadata>>,
	data:              Loadable<EntryData>,
	thumbnail_texture: Loadable<EntryImage>,
}

#[derive(Clone, derive_more::Debug)]
pub struct DirEntry(Arc<Inner>);

impl DirEntry {
	/// Creates a new directory entry
	pub(super) fn new(path: impl Into<Arc<Path>>) -> Self {
		Self(Arc::new(Inner {
			path:              Mutex::new(path.into()),
			metadata:          Loadable::new(),
			data:              Loadable::new(),
			thumbnail_texture: Loadable::new(),
		}))
	}
}

/// Path
impl DirEntry {
	/// Returns this entry's path
	pub fn path(&self) -> Arc<Path> {
		Arc::clone(&self.0.path.lock())
	}

	/// Renames this entry
	pub fn rename(&self, path: PathBuf) {
		*self.0.path.lock() = path.into();
	}
}

/// File name
impl DirEntry {
	/// Returns this entry's file name
	pub fn file_name(&self) -> Result<OsString, AppError> {
		// TODO: Avoid having to clone the file name
		let path = self.path();
		path.file_name().context("Missing file name").map(OsStr::to_owned)
	}
}

/// Metadata
impl DirEntry {
	/// Gets the metadata, blocking
	fn metadata_blocking(&self) -> Result<Arc<fs::Metadata>, AppError> {
		self.0
			.metadata
			.load(|| self::load_metadata(&self.path()))
			.context("Unable to get metadata")
	}

	/// Tries to gets the metadata
	fn try_metadata(&self, thread_pool: &PriorityThreadPool) -> Result<Option<Arc<fs::Metadata>>, AppError> {
		#[cloned(this = self)]
		self.0
			.metadata
			.try_load(thread_pool, Priority::DEFAULT, move || {
				self::load_metadata(&this.path())
			})
			.context("Unable to get metadata")
	}

	/// Sets the metadata of this entry
	pub(super) fn set_metadata(&self, metadata: fs::Metadata) {
		self.0.metadata.set(Arc::new(metadata));
	}
}

/// Data
impl DirEntry {
	/// Gets the entry's data, blocking
	fn data_blocking(&self, egui_ctx: &egui::Context) -> Result<EntryData, AppError> {
		self.0
			.data
			.load(|| self::load_entry_data(self.path(), egui_ctx))
			.context("Unable to get entry data")
	}

	/// Returns this entry's data
	pub fn try_data(
		&self,
		thread_pool: &PriorityThreadPool,
		egui_ctx: &egui::Context,
	) -> Result<Option<EntryData>, AppError> {
		#[cloned(this = self, egui_ctx)]
		self.0
			.data
			.try_load(thread_pool, Priority::HIGH, move || {
				self::load_entry_data(this.path(), &egui_ctx)
			})
			.context("Unable to get entry data")
	}

	/// Returns this entry's data
	pub fn data_if_exists(&self) -> Result<Option<EntryData>, AppError> {
		self.0.data.try_get().context("Unable to get entry data")
	}

	/// Removes this entry's data
	pub fn remove_data(&self) {
		self.0.data.remove();
	}
}

/// Modified date
impl DirEntry {
	/// Gets the modified date, blocking
	fn modified_date_blocking(&self) -> Result<SystemTime, AppError> {
		let metadata = self.metadata_blocking()?;
		metadata.modified().context("Unable to get modified date")
	}
}

/// Size
impl DirEntry {
	/// Gets the size, blocking
	fn size_blocking(&self) -> Result<u64, AppError> {
		let metadata = self.metadata_blocking()?;
		Ok(metadata.len())
	}

	/// Returns this image's file size
	pub fn try_size(&self, thread_pool: &PriorityThreadPool) -> Result<Option<u64>, AppError> {
		let Some(metadata) = self.try_metadata(thread_pool)? else {
			return Ok(None);
		};

		Ok(Some(metadata.len()))
	}
}

/// Thumbnail
impl DirEntry {
	/// Returns this image's thumbnail texture
	pub fn thumbnail_texture(
		&self,
		thread_pool: &PriorityThreadPool,
		egui_ctx: &egui::Context,
		thumbnails_dir: &Arc<Path>,
	) -> Result<Option<EntryImage>, AppError> {
		#[cloned(this = self, egui_ctx, thumbnails_dir)]
		self.0.thumbnail_texture.try_load(thread_pool, Priority::LOW, move || {
			let path = this.path();
			let data = this.data_blocking(&egui_ctx).context("Unable to get image kind")?;
			EntryImage::thumbnail(&egui_ctx, &thumbnails_dir, &path, &data)
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

/// Entry data
#[derive(Clone, Debug)]
pub enum EntryData {
	Image(EntryImage),
	Video(EntryVideo),
	Other,
}

fn load_metadata(path: &Path) -> Result<Arc<Metadata>, AppError> {
	let metadata = fs::metadata(path).context("Unable to get metadata")?;
	Ok(Arc::new(metadata))
}

fn load_entry_data(path: Arc<Path>, egui_ctx: &egui::Context) -> Result<EntryData, AppError> {
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
