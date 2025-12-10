//! Entry image

// Imports
use {
	crate::util::{AppError, Loadable, PriorityThreadPool, priority_thread_pool::Priority},
	app_error::Context,
	image::{DynamicImage, ImageFormat, ImageReader},
	std::{path::Path, sync::Arc},
	zutil_cloned::cloned,
};

#[derive(derive_more::Debug)]
struct Inner {
	path:   Arc<Path>,
	// TODO: Create a wrapper over `egui::TextureHandle` that implements Debug.
	#[debug("{:?}", self.handle.try_get().map(|res| res.as_ref().map(egui::TextureHandle::id)))]
	handle: Loadable<egui::TextureHandle>,
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
		let image = egui::ColorImage::from_rgba_unmultiplied(
			[image.width() as usize, image.height() as usize],
			&image.into_rgba8().into_flat_samples().samples,
		);
		let image = egui::ImageData::Color(Arc::new(image));

		// TODO: This filter should be customizable.
		let options = egui::TextureOptions::LINEAR;
		let handle = egui_ctx.load_texture(path.display().to_string(), image, options);

		Self {
			inner: Arc::new(Inner {
				path,
				handle: Loadable::loaded(handle),
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

			let image = egui::ColorImage::from_rgba_unmultiplied(
				[image.width() as usize, image.height() as usize],
				&image.into_rgba8().into_flat_samples().samples,
			);
			let image = egui::ImageData::Color(Arc::new(image));

			// TODO: This filter should be customizable.
			let options = egui::TextureOptions::LINEAR;
			let handle = egui_ctx.load_texture(path.display().to_string(), image, options);

			Ok(handle)
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
	pub fn handle(&self) -> Result<Option<egui::TextureHandle>, AppError> {
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
