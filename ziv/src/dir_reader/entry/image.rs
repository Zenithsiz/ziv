//! Entry image

// Imports
use {
	crate::util::{
		AppError,
		EguiCtxLoadImage,
		EguiTextureHandle,
		Loadable,
		PriorityThreadPool,
		priority_thread_pool::Priority,
	},
	app_error::Context,
	image::{DynamicImage, ImageFormat, ImageReader},
	std::{path::Path, sync::Arc},
	zutil_cloned::cloned,
};

#[derive(Debug)]
struct Inner {
	path:   Arc<Path>,
	handle: Loadable<EguiTextureHandle>,
	format: ImageFormat,
}

/// Entry image
#[derive(Clone, Debug)]
pub struct EntryImage {
	inner: Arc<Inner>,
}

impl EntryImage {
	/// Creates a new entry image from an image
	pub fn new(path: Arc<Path>, format: ImageFormat) -> Self {
		Self {
			inner: Arc::new(Inner {
				path,
				handle: Loadable::new(),
				format,
			}),
		}
	}

	/// Creates a loaded entry image
	pub fn loaded(path: Arc<Path>, format: ImageFormat, egui_ctx: &egui::Context, image: DynamicImage) -> Self {
		let handle = egui_ctx.load_image(path.display().to_string(), image);
		Self {
			inner: Arc::new(Inner {
				path,
				handle: Loadable::loaded(EguiTextureHandle(handle)),
				format,
			}),
		}
	}

	/// Starts loading this image, if unloaded
	pub fn load(&self, thread_pool: &PriorityThreadPool, egui_ctx: &egui::Context) -> Result<(), AppError> {
		#[cloned(path = self.inner.path, format = self.inner.format, egui_ctx;)]
		self.inner.handle.try_load(thread_pool, Priority::HIGH, move || {
			let mut image = self::open_with_format(&path, format)?;

			// Resize the image if it's too big for the gpu
			// Note: If the max texture size doesn't fit into a `u32`, then we can be sure
			//       that any image passed will fit.
			// TODO: Instead of decreasing the image size, can we just split into multiple images?
			let max_texture_size = egui_ctx.input(|input| input.max_texture_side);
			if let Ok(max_texture_size) = u32::try_from(max_texture_size) &&
				(image.width() > max_texture_size || image.height() > max_texture_size)
			{
				tracing::warn!(
					"Image size {}x{} did not fit into gpu's max texture size, resizing to fit \
					 {max_texture_size}x{max_texture_size}",
					image.width(),
					image.height()
				);
				image = image.resize(
					max_texture_size,
					max_texture_size,
					image::imageops::FilterType::Triangle,
				);
			}

			let handle = egui_ctx.load_image(path.display().to_string(), image);
			Ok(EguiTextureHandle(handle))
		})?;

		Ok(())
	}

	/// Unloads this image
	pub fn unload(&self) {
		self.inner.handle.remove();
	}

	/// Returns this image's format
	pub fn format(&self) -> ImageFormat {
		self.inner.format
	}

	/// Returns the image handle, if loaded
	pub fn handle(&self) -> Result<Option<EguiTextureHandle>, AppError> {
		self.inner.handle.try_get()
	}
}

/// Opens an image with a given format
// TODO: Move to util.
pub(super) fn open_with_format(path: &Path, format: ImageFormat) -> Result<image::DynamicImage, app_error::AppError> {
	let mut image_reader = ImageReader::open(path).context("Unable to open image")?;
	image_reader.set_format(format);
	image_reader.decode().context("Unable to read image")
}
