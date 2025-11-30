//! Directory entry

// Imports
use {
	super::{SortOrder, SortOrderKind},
	crate::{
		dirs::Dirs,
		util::{AppError, Loadable, PriorityThreadPool, priority_thread_pool::Priority},
	},
	app_error::{Context, app_error},
	core::{cmp::Ordering, hash::Hash, time::Duration},
	parking_lot::Mutex,
	std::{
		ffi::OsStr,
		fs::{self, Metadata},
		path::{Path, PathBuf},
		sync::Arc,
		time::SystemTime,
	},
	url::Url,
	zutil_cloned::cloned,
};

#[derive(derive_more::Debug)]
struct Inner {
	path: Mutex<Arc<Path>>,

	// TODO: Move all interior mutability inside of loadable to avoid holding
	//       the locks for longer than necessary
	metadata:          Mutex<Loadable<Arc<Metadata>>>,
	modified_date:     Mutex<Loadable<SystemTime>>,
	#[debug(ignore)]
	texture:           Mutex<Loadable<egui::TextureHandle>>,
	#[debug(ignore)]
	thumbnail_texture: Mutex<Loadable<egui::TextureHandle>>,
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
			modified_date:     Mutex::new(Loadable::new()),
			texture:           Mutex::new(Loadable::new()),
			thumbnail_texture: Mutex::new(Loadable::new()),
			image_details:     Mutex::new(None),
		}))
	}

	/// Returns this entry's path
	pub fn path(&self) -> Arc<Path> {
		Arc::clone(&*self.0.path.lock())
	}

	/// Returns this entry's file name
	pub fn file_name(&self) -> Result<String, AppError> {
		// TODO: Avoid having to clone the string?
		let path = self.path();
		path.file_name()
			.context("Missing file name")?
			.to_str()
			.map(str::to_owned)
			.context("File name was non-utf8")
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
	) -> Result<Option<egui::TextureHandle>, AppError> {
		#[cloned(this = self, egui_ctx)]
		self.0
			.texture
			.lock()
			.try_load(thread_pool, Priority::HIGH, move || {
				self::load_texture(&this.path(), &egui_ctx)
			})
			.map(Option::<&_>::cloned)
	}

	/// Removes this image's texture
	pub fn remove_texture(&self) {
		self.0.texture.lock().remove();
	}

	/// Returns this image's thumbnail texture
	pub fn thumbnail_texture(
		&self,
		thread_pool: &PriorityThreadPool,
		egui_ctx: &egui::Context,
		dirs: &Arc<Dirs>,
	) -> Result<Option<egui::TextureHandle>, AppError> {
		#[cloned(this = self, egui_ctx, dirs)]
		self.0
			.thumbnail_texture
			.lock()
			.try_load(thread_pool, Priority::LOW, move || {
				self::load_thumbnail_texture(&this.path(), &egui_ctx, &dirs)
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
			SortOrderKind::FileName => natord::compare(
				&self.file_name().context("Unable to get file name")?,
				&other.file_name().context("Unable to get file name")?,
			),
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

fn load_metadata(path: &Path) -> Result<Arc<Metadata>, AppError> {
	let metadata = fs::metadata(path).context("Unable to get metadata")?;
	Ok(Arc::new(metadata))
}

fn load_texture(path: &Path, egui_ctx: &egui::Context) -> Result<egui::TextureHandle, AppError> {
	let image = image::open(path).context("Unable to read image")?;

	let image = egui::ColorImage::from_rgba_unmultiplied(
		[image.width() as usize, image.height() as usize],
		&image.into_rgba8().into_flat_samples().samples,
	);
	let image = egui::ImageData::Color(Arc::new(image));

	// TODO: This filter should be customizable.
	let options = egui::TextureOptions::LINEAR;
	// TODO: Could we make it so we don't require an egui context here?
	let texture = egui_ctx.load_texture(path.display().to_string(), image, options);

	Ok::<_, AppError>(texture)
}

fn load_thumbnail_texture(path: &Path, egui_ctx: &egui::Context, dirs: &Dirs) -> Result<egui::TextureHandle, AppError> {
	let cache_path = {
		let path_absolute = path.canonicalize().context("Unable to canonicalize path")?;
		let path_uri = Url::from_file_path(&path_absolute)
			.map_err(|()| app_error!("Unable to turn path into url: {path_absolute:?}"))?;
		let path_md5 = md5::compute(path_uri.as_str());
		let thumbnail_file_name = format!("{path_md5:#x}.png");

		// TODO: Should we be using `png`s for the thumbnails?
		dirs.thumbnails().join(thumbnail_file_name)
	};

	let image = match image::open(&cache_path) {
		Ok(image) => image,
		Err(err) => {
			// Note: `image` supports gifs, so we can still generate thumbnails for them
			let is_video = path
				.extension()
				.and_then(OsStr::to_str)
				.is_some_and(|ext| ["webm", "mp4", "mkv"].contains(&ext));

			match is_video {
				true => {
					tracing::debug!(
						?path,
						?cache_path,
						?err,
						"No thumbnail found, but path was a video, so skipping thumbnail"
					);

					// TODO: Implement video thumbnails
					image::DynamicImage::new_rgba8(256, 256)
				},
				false => {
					tracing::debug!(?path, ?cache_path, ?err, "No thumbnail found, generating one");
					let image = image::open(path).context("Unable to read image")?;
					let thumbnail = image.thumbnail(256, 256);

					// TODO: Make saving the thumbnail a non-fatal error
					fs::create_dir_all(dirs.thumbnails()).context("Unable to create thumbnails directory")?;
					thumbnail.save(cache_path).context("Unable to save thumbnail")?;

					thumbnail
				},
			}
		},
	};


	let image = egui::ColorImage::from_rgba_unmultiplied(
		[image.width() as usize, image.height() as usize],
		&image.into_rgba8().into_flat_samples().samples,
	);
	let image = egui::ImageData::Color(Arc::new(image));

	// TODO: This filter should be customizable.
	let options = egui::TextureOptions::LINEAR;
	// TODO: Could we make it so we don't require an egui context here?
	let texture = egui_ctx.load_texture(path.display().to_string(), image, options);

	Ok::<_, AppError>(texture)
}
