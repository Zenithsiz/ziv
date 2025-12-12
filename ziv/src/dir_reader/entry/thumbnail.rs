//! Entry thumbnail

// Imports
use {
	super::{EntryData, EntryImage, video},
	crate::util::AppError,
	app_error::{Context, app_error},
	core::time::Duration,
	ffmpeg_next::Rescale,
	image::{DynamicImage, ImageFormat},
	std::path::Path,
	url::Url,
};

/// Entry thumbnail
#[derive(Clone, Debug)]
pub enum EntryThumbnail {
	Image(EntryImage),
	NonMedia,
}

impl EntryThumbnail {
	/// Creates a new thumbnail
	pub fn new(
		egui_ctx: &egui::Context,
		thumbnails_dir: &Path,
		entry_path: &Path,
		data: &EntryData,
	) -> Result<Self, AppError> {
		// Note: If the path is inside of the thumbnail cache, just load it to avoid recursively creating
		//       thumbnails of thumbnails.
		// TODO: This assumes that `thumbnails_dir` is absolute, which currently is true, but we shouldn't
		//       rely on that. On the other hand, canonicalizing it every time would be costly.
		// TODO: Instead of this, can we just check that the actual image is smaller than 256x256 and use it
		//       if it is?
		let entry_path_absolute = entry_path.canonicalize().context("Unable to canonicalize path")?;
		let cache_path = match entry_path_absolute.starts_with(thumbnails_dir) {
			true => entry_path.to_path_buf(),
			// Otherwise, get it's path in the cache
			false => {
				let path_uri = Url::from_file_path(&entry_path_absolute)
					.map_err(|()| app_error!("Unable to turn path into url: {entry_path_absolute:?}"))?;
				let path_md5 = md5::compute(path_uri.as_str());
				// TODO: Should we be using `png`s for the thumbnails?
				let thumbnail_file_name = format!("{path_md5:#x}.png");

				thumbnails_dir.join(thumbnail_file_name)
			},
		};

		let image = match image::open(&cache_path) {
			Ok(image) => image,
			Err(err) => {
				tracing::debug!(?entry_path, ?cache_path, ?err, "No thumbnail found, generating one");
				let thumbnail = match data {
					EntryData::Image(image) =>
						super::image::open_with_format(entry_path, image.format())?.thumbnail(256, 256),
					// Note: Despite `image` supporting GIFs, we create the thumbnail as a video
					EntryData::Video(_) => self::video_thumbnail(entry_path)?,
					EntryData::Other => return Ok(Self::NonMedia),
				};

				// TODO: Make saving the thumbnail a non-fatal error
				thumbnail.save(&cache_path).context("Unable to save thumbnail")?;

				thumbnail
			},
		};

		let image = EntryImage::loaded(cache_path.into(), ImageFormat::Png, egui_ctx, image);
		Ok(Self::Image(image))
	}
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
	let [input_width, input_height] = [decoder.width(), decoder.height()];
	app_error::ensure!(
		input_width != 0 && input_height != 0,
		"Video size was 0: {input_width}x{input_height}"
	);
	let output_size = match input_width > input_height {
		true => [256, 256 * input_height / input_width],
		false => [256 * input_width / input_height, 256],
	};
	let mut scaler = ffmpeg_next::software::scaling::context::Context::get(
		decoder.format(),
		input_width,
		input_height,
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

	// TODO: If the video is less than 2 seconds long this can happen, should
	//       we just retry with less time?
	app_error::bail!("Video had no packets");
}
