//! Entry thumbnail

// Imports
use {
	super::{EntryData, EntryDisplayGuess, EntrySource, ThumbnailDb, video},
	crate::{
		dir_reader::ThumbnailProgressGuard,
		util::{AppError, EguiCtxLoadImage, EguiTextureHandle},
	},
	app_error::Context,
	core::time::Duration,
	ffmpeg_next::Rescale,
	image::DynamicImage,
	std::path::Path,
};

/// Entry thumbnail
#[derive(Clone, Debug)]
pub enum EntryThumbnail {
	Image(EguiTextureHandle),
	NonMedia,
}

impl EntryThumbnail {
	/// Creates a new thumbnail
	pub fn new(
		egui_ctx: &egui::Context,
		thumbnails_db: &ThumbnailDb,
		source: &EntrySource,
		data: &EntryData,
		thumbnail_progress: &mut ThumbnailProgressGuard,
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

		let thumbnail = match thumbnails_db
			.thumbnail_image(&entry_path)
			.context("Unable to get thumbnail from database")?
		{
			// TODO: If the file is newer than our thumbnail, we should invalidate it.
			//       Unfortunately, we can't check this without loading the entry
			//       metadata, which isn't guaranteed to be loaded.
			Some((_date_added, thumbnail)) => thumbnail,
			None => {
				thumbnail_progress.set_generating();
				let EntryData::DisplayGuess(display_kind) = data else {
					return Ok(Self::NonMedia);
				};

				let (thumbnail, should_save) = match *display_kind {
					EntryDisplayGuess::Image { format } => {
						// If the image is thumbnail sized, just use it instead of generating a thumbnail
						let image = super::image::open_with_format(source, format)?;
						match image.width() <= 256 && image.height() <= 256 {
							true => {
								tracing::debug!(?entry_path, "Using image itself as the thumbnail");
								(image, false)
							},
							false => {
								tracing::debug!(?entry_path, "Generating thumbnail from image");
								let image = image.thumbnail(256, 256);
								(image, true)
							},
						}
					},
					EntryDisplayGuess::Video => match source {
						EntrySource::Path(path) => {
							tracing::debug!(?path, "Generating thumbnail from video");
							let image = self::video_thumbnail(path)?;
							(image, true)
						},
						EntrySource::Zip(_) =>
							app_error::bail!("Thumbnails of videos inside of a zip file aren't supported yet"),
					},
				};

				// Save it if we should
				if should_save {
					match thumbnails_db.add_thumbnail_image(&entry_path, DEFAULT_FORMAT, &thumbnail) {
						Ok(size) => tracing::info!(
							"Saved thumbnail for {entry_path:?} ({})",
							humansize::format_size(size, humansize::BINARY)
						),
						Err(err) => tracing::warn!("Unable to save thumbnail for {entry_path:?}: {err:?}"),
					}
				}

				thumbnail
			},
		};

		let texture = egui_ctx.load_image(format!("thumbnail://{}", entry_path.display()), thumbnail);
		Ok(Self::Image(texture))
	}
}

/// Thumbnail format
const DEFAULT_FORMAT: image::ImageFormat = image::ImageFormat::Jpeg;

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
		decoder.format() != ffmpeg_next::format::Pixel::None,
		"Video format was missing"
	);
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

	// Try to take a thumbnail at specific times
	let thumbnail_times = [
		Duration::from_secs(2),
		Duration::from_secs(1),
		Duration::from_millis(500),
		Duration::ZERO,
	];
	for thumbnail_time in thumbnail_times {
		let time_us = i64::try_from(thumbnail_time.as_micros()).context("Time as micros did not fit into an `i64`")?;
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
			let cur_time = thumbnail_time;
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
	}

	// Note: If none of the times worked, the video really had no packets
	app_error::bail!("Video had no packets");
}
