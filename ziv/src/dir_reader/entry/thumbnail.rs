//! Entry thumbnail

// Imports
use {
	super::{EntryData, EntryImage, EntrySource, video},
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
		source: &EntrySource,
		data: &EntryData,
	) -> Result<Self, AppError> {
		let entry_path = match source {
			EntrySource::Path(path) => path.canonicalize().context("Unable to canonicalize path")?,
			EntrySource::Zip(zip) => {
				let zip_path_absolute = zip
					.archive_path()
					.canonicalize()
					.context("Unable to canonicalize zip path")?;
				zip_path_absolute.join(zip.file_name())
			},
		};
		let entry_path_uri = Url::from_file_path(&entry_path)
			.map_err(|()| app_error!("Unable to turn path into url: {entry_path:?}"))?;
		let entry_path_md5 = md5::compute(entry_path_uri.as_str());
		// TODO: Should we be using `png`s for the thumbnails?
		let thumbnail_file_name = format!("{entry_path_md5:#x}.png");
		let thumbnail_path = thumbnails_dir.join(thumbnail_file_name);

		let (thumbnail, thumbnail_source) = match image::open(&thumbnail_path) {
			Ok(image) => (image, EntrySource::Path(thumbnail_path.into())),
			Err(_) => {
				let (thumbnail, thumbnail_source) = match data {
					EntryData::Image(image) => {
						// If the image is thumbnail sized, just use it instead of generating a thumbnail
						let image = super::image::open_with_format(source, image.format())?;
						match image.width() <= 256 && image.height() <= 256 {
							true => {
								tracing::debug!(source = ?source.name(), "Using image itself as the thumbnail");
								(image, source.clone())
							},
							false => {
								tracing::debug!(?thumbnail_path, source = ?source.name(), "Generating thumbnail from image");
								let thumbnail = image.thumbnail(256, 256);
								(thumbnail, EntrySource::Path(thumbnail_path.into()))
							},
						}
					},
					// Note: Despite `image` supporting GIFs, we create the thumbnail as a video
					EntryData::Video(_) => match source {
						EntrySource::Path(path) => {
							tracing::debug!(?thumbnail_path, ?path, "Generating thumbnail from video");

							let thumbnail = self::video_thumbnail(path)?;
							(thumbnail, EntrySource::Path(thumbnail_path.into()))
						},
						EntrySource::Zip(_) =>
							app_error::bail!("Thumbnails of videos inside of a zip file aren't supported yet"),
					},
					EntryData::Other => return Ok(Self::NonMedia),
				};

				// If we aren't using the image itself as a thumbnail, save it
				if let EntrySource::Path(thumbnail_path) = &thumbnail_source &&
					let EntrySource::Path(entry_path) = source &&
					thumbnail_path != entry_path
				{
					assert!(
						thumbnail_path.starts_with(thumbnails_dir),
						"Thumbnail path wasn't inside of thumbnail directory"
					);

					// TODO: Make saving the thumbnail a non-fatal error
					thumbnail.save(thumbnail_path).context("Unable to save thumbnail")?;
				}

				(thumbnail, thumbnail_source)
			},
		};

		let image = EntryImage::loaded(thumbnail_source, ImageFormat::Png, egui_ctx, thumbnail);
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
