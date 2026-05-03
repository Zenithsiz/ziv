//! Entry image

// Imports
use {
	super::{EntryResolution, EntrySource},
	crate::util::{AppError, Loadable, PriorityThreadPool, SmartTextureHandle, priority_thread_pool::Priority},
	app_error::Context,
	image::{DynamicImage, ImageFormat, ImageReader},
	std::{io, sync::Arc},
	zutil_cloned::cloned,
};

#[derive(Debug)]
struct Inner {
	source:     EntrySource,
	texture:    Loadable<SmartTextureHandle>,
	resolution: Loadable<EntryResolution>,
	format:     ImageFormat,
}

/// Entry image
#[derive(Clone, Debug)]
pub struct EntryImage {
	inner: Arc<Inner>,
}

impl EntryImage {
	/// Creates a new entry image from an image
	pub fn new(source: EntrySource, format: ImageFormat) -> Self {
		Self {
			inner: Arc::new(Inner {
				source,
				texture: Loadable::new(),
				resolution: Loadable::new(),
				format,
			}),
		}
	}

	/// Creates a loaded entry image
	pub fn loaded(source: EntrySource, format: ImageFormat, egui_ctx: &egui::Context, image: DynamicImage) -> Self {
		let texture = SmartTextureHandle::new(source.name(), image, egui_ctx);

		let [width, height] = texture.size();
		let resolution = EntryResolution { width, height };
		Self {
			inner: Arc::new(Inner {
				source,
				texture: Loadable::loaded(texture),
				resolution: Loadable::loaded(resolution),
				format,
			}),
		}
	}

	/// Starts loading this image, if unloaded
	pub fn load(&self, thread_pool: &PriorityThreadPool, egui_ctx: &egui::Context) -> Result<(), AppError> {
		#[cloned(source = self.inner.source, format = self.inner.format, egui_ctx;)]
		self.inner.texture.try_load(thread_pool, Priority::HIGH, move || try {
			let image = self::open_with_format(&source, format)?;
			SmartTextureHandle::new(source.name(), image, &egui_ctx)
		})?;

		Ok(())
	}

	/// Loads the resolution of this image
	fn load_resolution(&self) -> Result<EntryResolution, app_error::AppError> {
		match self.texture()? {
			Some(texture) => {
				let [width, height] = texture.size();
				Ok(EntryResolution { width, height })
			},
			None => {
				fn get_resolution<R: io::Seek + io::BufRead>(
					mut image_reader: ImageReader<R>,
					format: ImageFormat,
				) -> Result<EntryResolution, AppError> {
					image_reader.set_format(format);
					let (width, height) = image_reader.into_dimensions().context("Unable to read image")?;
					Ok(EntryResolution {
						width:  width as usize,
						height: height as usize,
					})
				}

				match &self.inner.source {
					EntrySource::Path(path) => {
						let image_reader = ImageReader::open(path).context("Unable to open image")?;
						get_resolution(image_reader, self.format())
					},
					EntrySource::Zip(zip) => {
						// TODO: Could we get away without reading the whole file?
						//       Unfortunately, `ImageReader` requires a `Seek`-able
						//       reader, but even a buffered zip file isn't seekable.
						let contents = zip.contents().context("Unable to read zip file contents")?;
						let image_reader = ImageReader::new(io::Cursor::new(contents));
						get_resolution(image_reader, self.format())
					},
				}
			},
		}
	}

	/// Gets this image's resolution, blocking
	pub fn resolution_blocking(&self) -> Result<EntryResolution, AppError> {
		self.inner.resolution.load(move || self.load_resolution())
	}

	/// Gets this image's resolution, loading it, if unloaded
	pub fn resolution_load(&self, thread_pool: &PriorityThreadPool) -> Result<Option<EntryResolution>, AppError> {
		#[cloned(this = self)]
		self.inner
			.resolution
			.try_load(thread_pool, Priority::DEFAULT, move || this.load_resolution())
	}

	/// Gets this image's resolution, if loaded
	pub fn resolution_if_loaded(&self) -> Result<Option<EntryResolution>, AppError> {
		self.inner.resolution.try_get()
	}

	/// Returns if this image's resolution is loaded
	pub fn resolution_is_loaded(&self) -> bool {
		self.inner.resolution.is_loaded()
	}

	/// Returns this image's format
	pub fn format(&self) -> ImageFormat {
		self.inner.format
	}

	/// Returns the image's texture, if loaded
	pub fn texture(&self) -> Result<Option<SmartTextureHandle>, AppError> {
		self.inner.texture.try_get()
	}
}

/// Opens an image with a given format
// TODO: Move to util.
pub(super) fn open_with_format(source: &EntrySource, format: ImageFormat) -> Result<DynamicImage, app_error::AppError> {
	fn read_image<R: io::Seek + io::BufRead>(
		mut image_reader: ImageReader<R>,
		format: ImageFormat,
	) -> Result<DynamicImage, AppError> {
		image_reader.set_format(format);
		image_reader.decode().context("Unable to read image")
	}

	match source {
		EntrySource::Path(path) => {
			let image_reader = ImageReader::open(path).context("Unable to open image")?;
			read_image(image_reader, format)
		},
		EntrySource::Zip(zip) => {
			// TODO: Could we get away without reading the whole file?
			//       Unfortunately, `ImageReader` requires a `Seek`-able
			//       reader, but even a buffered zip file isn't seekable.
			let contents = zip.contents().context("Unable to read zip file contents")?;
			let image_reader = ImageReader::new(io::Cursor::new(contents));
			read_image(image_reader, format)
		},
	}
}
