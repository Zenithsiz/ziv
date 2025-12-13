//! Entry video

// Imports
use {
	super::EntryResolution,
	crate::util::{
		AppError,
		EguiTextureHandle,
		InstantSaturatingOps,
		Loadable,
		PriorityThreadPool,
		TryControlFlow,
		priority_thread_pool::Priority,
	},
	app_error::Context,
	core::{mem, time::Duration},
	ffmpeg_next::Rescale,
	parking_lot::{Condvar, Mutex},
	std::{path::Path, sync::Arc, thread, time::Instant},
	zutil_cloned::cloned,
};

#[derive(Clone, Debug)]
pub enum PlayingStatus {
	Playing,
	Paused,
	Seeking {
		/// Whether we've already presented the frame we seeked to
		presented: bool,

		/// Previous status
		// TODO: Not box it and just indicate whether it was playing/paused?
		prev: Box<Self>,
	},
}

#[derive(Debug)]
struct State {
	texture_handle: Option<EguiTextureHandle>,
	thread_status:  DecoderThreadStatus,
	onscreen:       bool,
	playing_status: PlayingStatus,
	duration:       Option<Duration>,
	start_time:     Option<Instant>,
	cur_time:       Duration,
	seek_to:        Option<Duration>,
}

#[derive(Debug)]
struct Inner {
	path:       Arc<Path>,
	resolution: Loadable<EntryResolution>,
	state:      Mutex<State>,
	condvar:    Condvar,
}

/// Entry video
#[derive(Clone, Debug)]
pub struct EntryVideo {
	inner: Arc<Inner>,
}

impl EntryVideo {
	/// Creates a new video, not playing
	#[expect(clippy::unnecessary_wraps, reason = "We'll be fallible eventually")]
	pub fn new(path: Arc<Path>) -> Result<Self, AppError> {
		let inner = Arc::new(Inner {
			path,
			resolution: Loadable::new(),
			state: Mutex::new(State {
				texture_handle: None,
				thread_status:  DecoderThreadStatus::Stopped,
				onscreen:       false,
				playing_status: PlayingStatus::Playing,
				duration:       None,
				start_time:     None,
				cur_time:       Duration::ZERO,
				seek_to:        None,
			}),
			condvar: Condvar::new(),
		});

		Ok(Self { inner })
	}

	/// Starts the video
	pub fn start(&self, egui_ctx: egui::Context) {
		let mut state = self.inner.state.lock();
		if !matches!(state.thread_status, DecoderThreadStatus::Stopped) {
			return;
		}
		state.thread_status = DecoderThreadStatus::Started;

		#[cloned(inner = self.inner;)]
		let _ = thread::spawn(move || {
			let res: Result<_, AppError> = try {
				let mut thread = DecoderThread::new(inner)?;
				thread.run(&egui_ctx)?
			};

			match res {
				Ok(()) => tracing::debug!("Video decoder thread successfully returned"),
				Err(err) => tracing::warn!("Unable to decode video: {err:?}"),
			}
		});
	}

	/// Stops the video
	pub fn stop(&self) {
		let mut state = self.inner.state.lock();
		if matches!(state.thread_status, DecoderThreadStatus::Started) {
			state.thread_status = DecoderThreadStatus::Stopping;
			state.texture_handle = None;
			drop(state);
			self.inner.condvar.notify_one();
		}
	}

	/// Returns whether the video is started
	pub fn started(&self) -> bool {
		matches!(self.inner.state.lock().thread_status, DecoderThreadStatus::Started)
	}

	/// Loads the resolution of a video
	fn load_resolution(&self) -> Result<EntryResolution, app_error::AppError> {
		match self.size() {
			Some([width, height]) => Ok(EntryResolution { width, height }),
			None => {
				let input = ffmpeg_next::format::input(&self.inner.path).context("Unable to open video")?;
				let video_stream = input
					.streams()
					.best(ffmpeg_next::media::Type::Video)
					.context("No video streams found")?;

				// TODO: Do we actually have to create a decoder to get the dimensions?
				let decoder_ctx = ffmpeg_next::codec::context::Context::from_parameters(video_stream.parameters())
					.context("Unable to build decoder")?;
				let decoder = decoder_ctx.decoder().video().context("Unable to get video decoder")?;

				let width = decoder.width();
				let height = decoder.height();
				Ok(EntryResolution {
					width:  width as usize,
					height: height as usize,
				})
			},
		}
	}

	/// Gets this video's resolution, blocking
	pub fn _resolution_blocking(&self) -> Result<EntryResolution, AppError> {
		self.inner.resolution.load(move || self.load_resolution())
	}

	/// Gets this video's resolution, loading it
	pub fn resolution_load(&self, thread_pool: &PriorityThreadPool) -> Result<Option<EntryResolution>, AppError> {
		#[cloned(this = self)]
		self.inner
			.resolution
			.try_load(thread_pool, Priority::DEFAULT, move || this.load_resolution())
	}

	/// Gets this video's resolution, if loaded
	pub fn _resolution_if_loaded(&self) -> Result<Option<EntryResolution>, AppError> {
		self.inner.resolution.try_get()
	}

	/// Returns the size of the video
	pub fn size(&self) -> Option<[usize; 2]> {
		self.inner
			.state
			.lock()
			.texture_handle
			.as_ref()
			.map(|handle| handle.size())
	}

	/// Returns the handle
	pub fn handle(&self) -> Option<EguiTextureHandle> {
		self.inner.state.lock().texture_handle.clone()
	}

	/// Sets this video as on-screen.
	///
	/// Returns whether we were already on-screen
	pub fn set_onscreen(&self) -> bool {
		mem::replace(&mut self.inner.state.lock().onscreen, true)
	}

	/// Sets this video as off-screen
	///
	/// Returns whether we were on-screen
	pub fn set_offscreen(&self) -> bool {
		mem::replace(&mut self.inner.state.lock().onscreen, false)
	}

	/// Returns the video playing status
	pub fn playing_status(&self) -> PlayingStatus {
		self.inner.state.lock().playing_status.clone()
	}

	/// Pauses the video
	pub fn pause(&self) {
		self.inner.state.lock().playing_status = PlayingStatus::Paused;
		self.inner.condvar.notify_one();
	}

	/// Resumes the video
	pub fn resume(&self) {
		let mut state = self.inner.state.lock();
		state.playing_status = PlayingStatus::Playing;
		state.start_time = None;
		drop(state);
		self.inner.condvar.notify_one();
	}

	/// Starts seeking the video
	pub fn set_seeking(&self) {
		let mut state = self.inner.state.lock();
		match state.playing_status {
			PlayingStatus::Seeking { .. } => (),
			_ =>
				state.playing_status = PlayingStatus::Seeking {
					presented: false,
					prev:      Box::new(state.playing_status.clone()),
				},
		}
		drop(state);

		self.inner.condvar.notify_one();
	}

	/// Stops seeking the video
	pub fn stop_seeking(&self) {
		let mut state = self.inner.state.lock();
		if let PlayingStatus::Seeking { prev, .. } = state.playing_status.clone() {
			state.playing_status = *prev;
		}
		drop(state);

		self.inner.condvar.notify_one();
	}

	/// Returns the duration of the video, if any
	pub fn duration(&self) -> Option<Duration> {
		self.inner.state.lock().duration
	}

	/// Returns the current time of the video
	pub fn cur_time(&self) -> Duration {
		self.inner.state.lock().cur_time
	}

	/// Seeks to a time in the video.
	pub fn seek_to(&self, time: Duration) {
		let mut state = self.inner.state.lock();
		state.seek_to = Some(time);
		if let PlayingStatus::Seeking { presented, .. } = &mut state.playing_status {
			*presented = false;
		}
		drop(state);

		self.inner.condvar.notify_one();
	}
}

struct DecoderThread {
	inner: Arc<Inner>,

	input:   ffmpeg_next::format::context::Input,
	decoder: ffmpeg_next::decoder::Video,
	scaler:  ffmpeg_next::software::scaling::context::Context,

	video_stream_idx:       usize,
	video_stream_time_base: ffmpeg_next::Rational,
}

#[expect(
	clippy::match_same_arms,
	clippy::needless_continue,
	reason = "We want control flow to be explicit here"
)]
impl DecoderThread {
	/// Creates the decoder thread state
	fn new(inner: Arc<Inner>) -> Result<Self, AppError> {
		// Open the input
		let input = ffmpeg_next::format::input(&inner.path).context("Unable to open video")?;

		// Get the video stream
		let video_stream = input
			.streams()
			.best(ffmpeg_next::media::Type::Video)
			.context("No video streams found")?;
		let video_stream_idx = video_stream.index();
		let video_stream_time_base = video_stream.time_base();

		// Write it's duration
		let duration_us = input.duration().rescale(TIME_BASE_AV, TIME_BASE_MICROS);
		if let Ok(duration_us) = u64::try_from(duration_us) {
			let duration = Duration::from_micros(duration_us);
			inner.state.lock().duration = Some(duration);
		}

		// Create a decoder
		let decoder_ctx = ffmpeg_next::codec::context::Context::from_parameters(video_stream.parameters())
			.context("Unable to build decoder")?;
		let decoder = decoder_ctx.decoder().video().context("Unable to get video decoder")?;

		// And a scaler
		let scaler = ffmpeg_next::software::scaling::context::Context::get(
			decoder.format(),
			decoder.width(),
			decoder.height(),
			ffmpeg_next::format::Pixel::RGBA,
			decoder.width(),
			decoder.height(),
			ffmpeg_next::software::scaling::Flags::BILINEAR,
		)
		.context("Unable to build scaler")?;

		Ok(Self {
			inner,
			input,
			decoder,
			scaler,
			video_stream_idx,
			video_stream_time_base,
		})
	}

	/// Runs the thread
	fn run(&mut self, egui_ctx: &egui::Context) -> Result<(), AppError> {
		// Note: This loop plays the video, rewinding once it reaches the end
		loop {
			// Note: This loops reads the packets and frames until eof
			loop {
				// Note: Even if we just seeked, we can just continue
				let _events = self.handle_events()?;

				// Get the next packet for the video stream
				let Some((stream, packet)) = self.input.packets().next() else {
					break;
				};
				if stream.index() != self.video_stream_idx {
					continue;
				}

				// Send the packet to the decoder and receive it's frames
				// Note: If `frame_res` is `Seeked`, we can just continue.
				self.decoder
					.send_packet(&packet)
					.context("Unable to send packet to decoder")?;
				match self.receive_frames(egui_ctx)? {
					// If we seeked, we can just go back to getting packets
					FrameRes::Seeked => continue,
					FrameRes::DecoderWaitPacket => continue,
					FrameRes::DecoderEof => unreachable!("Received unexpected EOF from decoder"),
				}
			}

			// After reaching eof, send it to the decoder,
			// and receive any remaining frames
			self.decoder.send_eof().context("Unable to send eof to decoder")?;
			match self.receive_frames(egui_ctx)? {
				// If we seeked, just continue getting frames again
				FrameRes::Seeked => continue,
				FrameRes::DecoderWaitPacket => unreachable!("Decoder was expected frames after EOF"),
				// If we reached EOF, rewind back to the beginning
				FrameRes::DecoderEof => self.seek_to(Duration::ZERO).context("Unable to rewind input")?,
			}
		}
	}

	fn receive_frames(&mut self, egui_ctx: &egui::Context) -> TryControlFlow<FrameRes, (), AppError> {
		let mut frame_raw = ffmpeg_next::frame::Video::empty();
		loop {
			// Wait until we're unpaused.
			match self.wait_unpaused()? {
				// Note: If we seeked, we'll need to get more frames, so return
				WaitRes::Seeked => return TryControlFlow::Continue(FrameRes::Seeked),
				WaitRes::Finished => (),
			}

			// Then receive the frame
			match self.decoder.receive_frame(&mut frame_raw) {
				Ok(()) => (),
				Err(ffmpeg_next::Error::Other { errno: libc::EAGAIN }) =>
					return TryControlFlow::Continue(FrameRes::DecoderWaitPacket),
				Err(ffmpeg_next::Error::Eof) => return TryControlFlow::Continue(FrameRes::DecoderEof),
				Err(err) => return TryControlFlow::BreakErr(AppError::new(&err).context("Decoder returned an error")),
			}

			// Get and adjust the pts
			// TODO: What does it mean for a frame to not have a pts?
			//       Should we be using dts instead?
			let Some(pts_raw) = frame_raw.pts() else {
				tracing::warn!("Frame had no pts, skipping");
				continue;
			};
			let pts_us = pts_raw.rescale(self.video_stream_time_base, TIME_BASE_MICROS);
			let Ok(pts_us) = u64::try_from(pts_us) else {
				tracing::warn!("Frame pts was negative: {pts_us}, skipping");
				continue;
			};
			let pts = Duration::from_micros(pts_us);

			// If the frame is in the past, discard it
			// Note: This can happen because the seeks aren't exact, and so
			//       we might have been put a bit before our actual time.
			let cur_time = self.inner.state.lock().cur_time;
			if pts < cur_time {
				continue;
			}

			// Get when the frame starts and sleep until it does
			let Some(frame_start_time) = self
				.inner
				.state
				.lock()
				.start_time
				.get_or_insert_with(|| Instant::now().saturating_sub(cur_time))
				.checked_add(pts)
			else {
				tracing::warn!("Frame pts overflowed an instant: {pts:?}, skipping");
				continue;
			};

			// Wait until the frame starts before rendering
			// Note: If we just seeked, discard it and return
			match self.wait_until(frame_start_time)? {
				WaitRes::Seeked => return TryControlFlow::Continue(FrameRes::Seeked),
				WaitRes::Finished => (),
			}
			self.inner.state.lock().cur_time = pts;

			// Once we arrive, update the current time and send the frame back
			let mut frame = ffmpeg_next::frame::Video::empty();
			self.scaler
				.run(&frame_raw, &mut frame)
				.context("Unable to scale frame")?;

			let width = frame.width() as usize;
			let height = frame.height() as usize;
			let size = [width, height];
			let mut pixels = Vec::with_capacity(width * height);

			let data = frame.data(0);
			let stride = frame.stride(0);
			for y in 0..height {
				let start = y * stride;
				let data = &data[start..][..4 * width];
				pixels.extend(
					data.iter()
						.copied()
						.array_chunks()
						.map(|[r, g, b, a]| egui::Color32::from_rgba_premultiplied(r, g, b, a)),
				);
			}
			let image = egui::ColorImage {
				size,
				source_size: egui::vec2(width as f32, height as f32),
				pixels,
			};

			// TODO: This filter should be customizable
			let options = egui::TextureOptions::LINEAR;
			let handle = self.inner.state.lock().texture_handle.clone();
			match handle {
				Some(mut handle) => handle.set(image, options),
				None => {
					let handle = egui_ctx.load_texture(self.inner.path.display().to_string(), image, options);
					self.inner.state.lock().texture_handle = Some(EguiTextureHandle(handle));
				},
			}

			let mut state = self.inner.state.lock();
			if let PlayingStatus::Seeking { presented, .. } = &mut state.playing_status {
				*presented = true;
			}
		}
	}

	fn seek_to(&mut self, time: Duration) -> Result<(), AppError> {
		// Seek on the input and flush the decoder
		// Note: `input.seek` uses `-1` for the stream index, so the time base is `AV_TIME_BASE`.
		let time_us = i64::try_from(time.as_micros()).context("Time as micros did not fit into an `i64`")?;
		let time_us = time_us.rescale(TIME_BASE_MICROS, TIME_BASE_AV);
		self.input.seek(time_us, ..time_us).context("Unable to seek input")?;
		self.decoder.flush();

		// Then set the correct time on ourselves
		let mut state = self.inner.state.lock();
		state.start_time = None;
		state.cur_time = time;

		Ok(())
	}

	/// Handles events, returning any user event
	fn handle_events(&mut self) -> TryControlFlow<UserEvent, (), AppError> {
		// Check if we should quit.
		let should_quit = self.inner.state.lock().thread_status == DecoderThreadStatus::Stopping;
		if should_quit {
			return TryControlFlow::BreakOk(());
		}

		let mut events = UserEvent::empty();

		// Check if we need to seek
		// TODO: Should we make the caller seek instead of us?
		let (seek_to, cur_time) = {
			let mut state = self.inner.state.lock();
			(state.seek_to.take(), state.cur_time)
		};
		if let Some(time) = seek_to &&
			time != cur_time
		{
			events |= UserEvent::SEEK;

			if let Err(err) = self.seek_to(time) {
				tracing::warn!("Unable to seek to {time:?}: {err:?}");
			}
		}

		TryControlFlow::Continue(events)
	}

	/// Waits until `end`
	fn wait_until(&mut self, end: Instant) -> TryControlFlow<WaitRes, (), AppError> {
		loop {
			let events = self.handle_events()?;
			if events.contains(UserEvent::SEEK) {
				break TryControlFlow::Continue(WaitRes::Seeked);
			}

			let res = self.inner.condvar.wait_until(&mut self.inner.state.lock(), end);
			if res.timed_out() {
				break TryControlFlow::Continue(WaitRes::Finished);
			}
		}
	}

	/// Waits until unpaused
	fn wait_unpaused(&mut self) -> TryControlFlow<WaitRes, (), AppError> {
		loop {
			let events = self.handle_events()?;
			if events.contains(UserEvent::SEEK) {
				break TryControlFlow::Continue(WaitRes::Seeked);
			}

			let mut state = self.inner.state.lock();
			match state.playing_status {
				PlayingStatus::Playing | PlayingStatus::Seeking { presented: false, .. } =>
					break TryControlFlow::Continue(WaitRes::Finished),
				PlayingStatus::Paused | PlayingStatus::Seeking { presented: true, .. } =>
					self.inner.condvar.wait(&mut state),
			}
		}
	}
}

impl Drop for DecoderThread {
	fn drop(&mut self) {
		let mut state = self.inner.state.lock();
		state.thread_status = DecoderThreadStatus::Stopped;
	}
}

pub(super) const TIME_BASE_AV: ffmpeg_next::Rational = ffmpeg_next::Rational(1, ffmpeg_next::ffi::AV_TIME_BASE);
pub(super) const TIME_BASE_MICROS: ffmpeg_next::Rational = ffmpeg_next::Rational(1, 1_000_000);

/// Decoder thread status
#[derive(PartialEq, Eq, Clone, Copy, Debug)]
enum DecoderThreadStatus {
	Stopped,
	Started,
	Stopping,
}

/// Result from decoding a frame
#[derive(PartialEq, Eq, Clone, Copy, Debug)]
#[must_use]
enum FrameRes {
	/// User seeked
	Seeked,

	/// Decoder needs more packets
	DecoderWaitPacket,

	/// Decoder received an eof
	DecoderEof,
}

/// Result from waiting
#[derive(PartialEq, Eq, Clone, Copy, Debug)]
#[must_use]
enum WaitRes {
	/// Seeked while waiting
	Seeked,

	/// Finished waiting
	Finished,
}

bitflags::bitflags! {
	/// User events
	#[derive(PartialEq, Eq, Clone, Copy, Debug)]
	#[must_use]
	struct UserEvent: u32 {
		const SEEK = 1 << 0;
	}
}
