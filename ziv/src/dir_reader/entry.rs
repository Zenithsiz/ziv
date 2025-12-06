//! Directory entry

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
	core::{cmp::Ordering, hash::Hash, time::Duration},
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

	// TODO: Move all interior mutability inside of loadable to avoid holding
	//       the locks for longer than necessary
	metadata:          Mutex<Loadable<Arc<Metadata>>>,
	image_kind:        Mutex<Loadable<ImageKind>>,
	modified_date:     Mutex<Loadable<SystemTime>>,
	texture:           Mutex<Loadable<EntryImage>>,
	video:             Mutex<Loadable<EntryVideo>>,
	thumbnail_texture: Mutex<Loadable<EntryImage>>,
	image_details:     Mutex<Option<ImageDetails>>,
}

#[derive(Clone, derive_more::Debug)]
pub struct DirEntry(Arc<Inner>);

impl DirEntry {
	/// Creates a new directory entry
	pub(super) fn new(path: impl Into<Arc<Path>>) -> Self {
		Self(Arc::new(Inner {
			path:              Mutex::new(path.into()),
			metadata:          Mutex::new(Loadable::new()),
			image_kind:        Mutex::new(Loadable::new()),
			modified_date:     Mutex::new(Loadable::new()),
			texture:           Mutex::new(Loadable::new()),
			video:             Mutex::new(Loadable::new()),
			thumbnail_texture: Mutex::new(Loadable::new()),
			image_details:     Mutex::new(None),
		}))
	}

	/// Returns this entry's path
	pub fn path(&self) -> Arc<Path> {
		Arc::clone(&*self.0.path.lock())
	}

	/// Returns this entry's file name
	pub fn file_name(&self) -> Result<OsString, AppError> {
		// TODO: Avoid having to clone the file name
		let path = self.path();
		path.file_name().context("Missing file name").map(OsStr::to_owned)
	}

	/// Renames this entry
	pub fn rename(&self, path: PathBuf) {
		*self.0.path.lock() = path.into();
	}

	/// Gets the metadata, blocking
	fn metadata_blocking(&self) -> Result<Arc<fs::Metadata>, AppError> {
		self.0
			.metadata
			.lock()
			.load(|| self::load_metadata(&self.path()))
			.context("Unable to get metadata")
			.map(Arc::clone)
	}

	/// Tries to gets the metadata
	fn try_metadata(&self, thread_pool: &PriorityThreadPool) -> Result<Option<Arc<fs::Metadata>>, AppError> {
		#[cloned(this = self)]
		self.0
			.metadata
			.lock()
			.try_load(thread_pool, Priority::DEFAULT, move || {
				self::load_metadata(&this.path())
			})
			.map(|res| res.map(Arc::clone))
			.context("Unable to get metadata")
	}

	/// Sets the metadata of this entry
	pub(super) fn set_metadata(&self, metadata: fs::Metadata) {
		self.0.metadata.lock().set(Arc::new(metadata));
	}

	/// Gets the image kind, blocking
	fn image_kind_blocking(&self) -> Result<ImageKind, AppError> {
		self.0
			.image_kind
			.lock()
			.load(|| self::load_image_kind(&self.path()))
			.context("Unable to get image kind")
			.copied()
	}

	/// Returns this image's kind
	pub fn try_image_kind(&self, thread_pool: &PriorityThreadPool) -> Result<Option<ImageKind>, AppError> {
		#[cloned(this = self)]
		self.0
			.image_kind
			.lock()
			.try_load(thread_pool, Priority::HIGH, move || self::load_image_kind(&this.path()))
			.map(Option::<&_>::copied)
			.context("Unable to get image kind")
	}

	/// Gets the modified date, blocking
	fn modified_date_blocking(&self) -> Result<SystemTime, AppError> {
		self.0
			.modified_date
			.lock()
			.load(|| {
				let metadata = self.metadata_blocking()?;
				metadata.modified().context("Unable to get modified date")
			})
			.copied()
	}

	/// Returns the image details of this entry
	pub fn image_details(&self) -> Option<ImageDetails> {
		self.0.image_details.lock().clone()
	}

	/// Returns if this entry contains image details
	pub fn has_image_details(&self) -> bool {
		self.0.image_details.lock().is_some()
	}

	/// Sets the image details of this entry
	pub fn set_image_details(&self, image_details: ImageDetails) {
		*self.0.image_details.lock() = Some(image_details);
	}

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

	/// Returns this image's texture
	pub fn texture(
		&self,
		thread_pool: &PriorityThreadPool,
		egui_ctx: &egui::Context,
	) -> Result<Option<EntryImage>, AppError> {
		#[cloned(this = self, egui_ctx)]
		self.0
			.texture
			.lock()
			.try_load(thread_pool, Priority::HIGH, move || {
				let ImageKind::Image { format } = this.image_kind_blocking().context("Unable to get image kind")?
				else {
					app_error::bail!("Cannot load a video's texture");
				};
				EntryImage::new(&egui_ctx, &this.path(), format)
			})
			.map(Option::<&_>::cloned)
	}

	/// Removes this image's texture
	pub fn remove_texture(&self) {
		self.0.texture.lock().remove();
	}

	/// Returns this image's video
	pub fn video(
		&self,
		thread_pool: &PriorityThreadPool,
		egui_ctx: &egui::Context,
	) -> Result<Option<EntryVideo>, AppError> {
		#[cloned(this = self, egui_ctx)]
		self.0
			.video
			.lock()
			.try_load(thread_pool, Priority::HIGH, move || {
				EntryVideo::new(&egui_ctx, &this.path())
			})
			.map(Option::<&_>::cloned)
	}

	/// Removes this image's video
	pub fn remove_video(&self) {
		self.0.video.lock().remove();
	}

	/// Returns this image's thumbnail texture
	pub fn thumbnail_texture(
		&self,
		thread_pool: &PriorityThreadPool,
		egui_ctx: &egui::Context,
		thumbnails_dir: &Arc<Path>,
	) -> Result<Option<EntryImage>, AppError> {
		#[cloned(this = self, egui_ctx, thumbnails_dir)]
		self.0
			.thumbnail_texture
			.lock()
			.try_load(thread_pool, Priority::LOW, move || {
				let kind = this.image_kind_blocking().context("Unable to get image kind")?;
				EntryImage::thumbnail(&egui_ctx, &thumbnails_dir, &this.path(), kind)
			})
			.map(Option::<&_>::cloned)
	}

	/// Removes this image's thumbnail texture
	pub fn _remove_thumbnail_texture(&self) {
		self.0.texture.lock().remove();
	}

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

/// Image details for an entry
#[derive(Clone, Debug)]
pub enum ImageDetails {
	Image { size: egui::Vec2 },
	Video { size: egui::Vec2, duration: Duration },
}

/// Image kind
#[derive(Clone, Copy, Debug)]
pub enum ImageKind {
	Image { format: ImageFormat },
	Video,
}

fn load_metadata(path: &Path) -> Result<Arc<Metadata>, AppError> {
	let metadata = fs::metadata(path).context("Unable to get metadata")?;
	Ok(Arc::new(metadata))
}

fn load_image_kind(path: &Path) -> Result<ImageKind, AppError> {
	// TODO: Instead of only accepting a set list of extensions for video, can
	//       we use `ffprobe` to check whether it's a video or not?
	if let Some(ext) = path.extension().and_then(OsStr::to_str) &&
		matches!(ext, "gif" | "mkv" | "mp4" | "webm")
	{
		return Ok(ImageKind::Video);
	}

	// If we got a format just from the path, return it
	// TODO: Should we trust the extension?
	if let Some(ext) = path.extension() &&
		let Some(format) = ImageFormat::from_extension(ext)
	{
		return Ok(ImageKind::Image { format });
	}

	// Otherwise, try to guess it by opening it
	let reader = ::image::ImageReader::open(path)
		.context("Unable to create image reader")?
		.with_guessed_format()
		.context("Unable to read file")?;
	if let Some(format) = reader.format() {
		return Ok(ImageKind::Image { format });
	}

	app_error::bail!("Unable to guess image kind");
}
