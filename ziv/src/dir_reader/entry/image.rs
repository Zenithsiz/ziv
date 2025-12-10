//! Entry image

// Imports
use {
	super::EntryData,
	crate::{
		dir_reader::entry::video,
		util::{AppError, Loadable, PriorityThreadPool, priority_thread_pool::Priority},
	},
	app_error::{Context, app_error},
	core::time::Duration,
	ffmpeg_next::Rescale,
	image::{DynamicImage, ImageFormat, ImageReader},
	std::{path::Path, sync::Arc},
	url::Url,
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
// TODO: Make this lazy-loaded
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

	/// Creates a new thumbnail entry image from an image.
	///
	/// Eagerly loads the thumbnail.
	pub fn thumbnail(
		egui_ctx: &egui::Context,
		thumbnails_dir: &Path,
		path: &Path,
		data: &EntryData,
	) -> Result<Self, AppError> {
		// Note: If the path is inside of the thumbnail cache, just load it to avoid recursively creating
		//       thumbnails of thumbnails.
		// TODO: This assumes that `thumbnails_dir` is absolute, which currently is true, but we shouldn't
		//       rely on that. On the other hand, canonicalizing it every time would be costly.
		// TODO: Instead of this, can we just check that the actual image is smaller than 256x256 and use it
		//       if it is?
		let path_absolute = path.canonicalize().context("Unable to canonicalize path")?;
		let cache_path = match path_absolute.starts_with(thumbnails_dir) {
			true => path.to_path_buf(),
			// Otherwise, get it's path in the cache
			false => {
				let path_uri = Url::from_file_path(&path_absolute)
					.map_err(|()| app_error!("Unable to turn path into url: {path_absolute:?}"))?;
				let path_md5 = md5::compute(path_uri.as_str());
				// TODO: Should we be using `png`s for the thumbnails?
				let thumbnail_file_name = format!("{path_md5:#x}.png");

				thumbnails_dir.join(thumbnail_file_name)
			},
		};

		let image = match image::open(&cache_path) {
			Ok(image) => image,
			Err(err) => {
				tracing::debug!(?path, ?cache_path, ?err, "No thumbnail found, generating one");
				let thumbnail = match data {
					EntryData::Image(image) => self::open_with_format(path, image.format())?.thumbnail(256, 256),
					// Note: Despite `image` supporting GIFs, we create the thumbnail as a video
					EntryData::Video(_) => self::video_thumbnail(path)?,
				};

				// TODO: Make saving the thumbnail a non-fatal error
				thumbnail.save(&cache_path).context("Unable to save thumbnail")?;

				thumbnail
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
		let handle = egui_ctx.load_texture(path.display().to_string(), image, options);

		Ok(Self {
			inner: Arc::new(Inner {
				path:   cache_path.into(),
				handle: Loadable::loaded(handle),
				format: ImageFormat::Png,
			}),
		})
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
fn open_with_format(path: &Path, format: ImageFormat) -> Result<image::DynamicImage, app_error::AppError> {
	let mut image_reader = ImageReader::open(path).context("Unable to open image")?;
	image_reader.set_format(format);
	image_reader.decode().context("Unable to read image")
}

/// Creates a thumbnail for a video
// TODO: Deduplicate this with `super::video`.
pub fn video_thumbnail(path: &Path) -> Result<DynamicImage, AppError> {
	// Open the input
	let mut input = ffmpeg_next::format::input(path).context("Unable to open video")?;

	// Get the video stream
	let video_stream = input
		.streams()
		.best(ffmpeg_next::media::Type::Video)
		.context("No video streams found")?;
	let video_stream_idx = video_stream.index();
	let video_stream_time_base = video_stream.time_base();

	// Create a decoder
	let decoder_ctx = ffmpeg_next::codec::context::Context::from_parameters(video_stream.parameters())
		.context("Unable to build decoder")?;
	let mut decoder = decoder_ctx.decoder().video().context("Unable to get video decoder")?;

	// And a scaler
	let input_size = [decoder.width(), decoder.height()];
	let output_size = match input_size[0] > input_size[1] {
		true => [256, 256 * input_size[1] / input_size[0]],
		false => [256 * input_size[0] / input_size[1], 256],
	};
	let mut scaler = ffmpeg_next::software::scaling::context::Context::get(
		decoder.format(),
		input_size[0],
		input_size[1],
		ffmpeg_next::format::Pixel::RGBA,
		output_size[0],
		output_size[1],
		ffmpeg_next::software::scaling::Flags::BILINEAR,
	)
	.context("Unable to build scaler")?;

	// Seek to 2 second into for the thumbnail
	// TODO: Adjust this time?
	let time = Duration::from_secs(2);
	let time_us = i64::try_from(time.as_micros()).context("Time as micros did not fit into an `i64`")?;
	let time_us = time_us.rescale(video::TIME_BASE_MICROS, video::TIME_BASE_AV);
	input.seek(time_us, ..time_us).context("Unable to seek input")?;

	for (stream, packet) in input.packets() {
		if stream.index() != video_stream_idx {
			continue;
		}

		decoder
			.send_packet(&packet)
			.context("Unable to send packet to decoder")?;

		let mut frame_raw = ffmpeg_next::frame::Video::empty();
		match decoder.receive_frame(&mut frame_raw) {
			Ok(()) => (),
			Err(ffmpeg_next::Error::Other { errno: libc::EAGAIN }) => continue,
			Err(err) => return Err(AppError::new(&err).context("Decoder returned an error")),
		}

		// Get and adjust the pts
		// TODO: What does it mean for a frame to not have a pts?
		//       Should we be using dts instead?
		let Some(pts_raw) = frame_raw.pts() else {
			tracing::warn!("Frame had no pts, skipping");
			continue;
		};
		let pts_us = pts_raw.rescale(video_stream_time_base, video::TIME_BASE_MICROS);
		let Ok(pts_us) = u64::try_from(pts_us) else {
			tracing::warn!("Frame pts was negative: {pts_us}, skipping");
			continue;
		};
		let pts = Duration::from_micros(pts_us);

		// If the frame is in the past, discard it
		// Note: This can happen because the seeks aren't exact, and so
		//       we might have been put a bit before our actual time.
		let cur_time = time;
		if pts < cur_time {
			continue;
		}

		let mut frame = ffmpeg_next::frame::Video::empty();
		scaler.run(&frame_raw, &mut frame).context("Unable to scale frame")?;

		let width = frame.width() as usize;
		let height = frame.height() as usize;
		let mut pixels = Vec::with_capacity(width * height);

		let data = frame.data(0);
		let stride = frame.stride(0);
		for y in 0..height {
			let start = y * stride;
			let data = &data[start..][..4 * width];
			pixels.extend(data.iter().copied());
		}

		let image =
			image::RgbaImage::from_vec(frame.width(), frame.height(), pixels).context("Image buffer was wrong")?;
		return Ok(image.into());
	}

	// TODO: If the video is less than 1 second this can happen, should
	//       we just retry with less time?
	app_error::bail!("Video had no packets");
}
