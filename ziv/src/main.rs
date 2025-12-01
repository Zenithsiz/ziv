//! Zenithsiz's image viewer

// Features
#![feature(
	proc_macro_hygiene,
	once_cell_try,
	stmt_expr_attributes,
	try_blocks,
	arbitrary_self_types,
	iter_array_chunks,
	exact_size_is_empty,
	decl_macro,
	range_into_bounds,
	path_is_empty,
	yeet_expr
)]

// Modules
mod args;
mod dir_reader;
mod dirs;
mod shortcut;
mod util;

// Imports
use {
	self::{
		args::Args,
		dir_reader::{CurEntry, DirEntry, DirReader, SortOrder, SortOrderKind, entry::ImageDetails},
		dirs::Dirs,
		shortcut::Shortcuts,
		util::{AppError, PriorityThreadPool, RectUtils},
	},
	app_error::Context,
	clap::Parser,
	core::time::Duration,
	directories::ProjectDirs,
	egui::{Widget, emath::GuiRounding},
	indexmap::IndexSet,
	itertools::Itertools,
	std::{ffi::OsStr, fmt::Write, path::PathBuf, process::ExitCode, sync::Arc},
	strum::VariantArray,
	zutil_logger::Logger,
};


fn main() -> ExitCode {
	match self::run() {
		Ok(()) => ExitCode::SUCCESS,
		Err(err) => {
			tracing::error!("{}", err.pretty());
			ExitCode::FAILURE
		},
	}
}

fn run() -> Result<(), AppError> {
	// Initialize logging
	let logger = {
		let default_filters = |default| [(None, default)];
		Logger::new(std::io::stderr, (), default_filters("info"), default_filters("debug"))
	};

	// Parse arguments
	let args = Args::parse();
	tracing::debug!(?args, "Arguments");

	let dirs = ProjectDirs::from("", "", "ziv").context("Unable to create app directories")?;
	let dirs = Dirs::new(dirs.cache_dir().to_owned());
	let dirs = Arc::new(dirs);

	// Set logger file from arguments
	logger.set_file(args.log_file.as_deref());

	let thread_pool = PriorityThreadPool::new().context("Unable to create thread pool")?;

	let path = match args.path {
		Some(path) => path,
		None => std::env::current_dir().context("Unable to get current directory")?,
	};
	let native_options = eframe::NativeOptions::default();
	eframe::run_native(
		"ziv",
		native_options,
		Box::new(|cc| {
			let app = EguiApp::new(cc, dirs, thread_pool, path);
			Ok(Box::new(app))
		}),
	)
	.context("Unable to create window")?;

	Ok(())
}

#[derive(derive_more::Debug)]
struct CurPlayer {
	entry:  DirEntry,
	#[debug(ignore)]
	player: egui_video::Player,
}

#[derive(PartialEq, Eq, Clone, Copy, Hash, Debug)]
#[derive(strum::VariantArray)]
#[derive(serde::Serialize, serde::Deserialize)]
enum ViewMode {
	FitWindow,
	FitWidth,
	ActualSize,
}

#[derive(PartialEq, Eq, Clone, Copy, Hash, Debug)]
#[derive(strum::VariantArray)]
#[derive(serde::Serialize, serde::Deserialize)]
enum DisplayMode {
	Image,
	List,
}
impl DisplayMode {
	/// Returns the other variant of this display
	const fn toggle(self) -> Self {
		match self {
			Self::Image => Self::List,
			Self::List => Self::Image,
		}
	}
}

#[derive(Debug)]
struct EguiApp {
	dirs:            Arc<Dirs>,
	thread_pool:     PriorityThreadPool,
	dir_reader:      DirReader,
	cur_player:      Option<CurPlayer>,
	next_frame_idx:  usize,
	pan_zoom:        PanZoom,
	resized_image:   bool,
	view_mode:       ViewMode,
	display_mode:    DisplayMode,
	shortcuts:       Shortcuts,
	entries_per_row: usize,

	/// Entries we're loaded
	loaded_entries:     IndexSet<DirEntry>,
	max_loaded_entries: usize,
}

impl EguiApp {
	/// Creates a new app
	pub fn new(
		cc: &eframe::CreationContext<'_>,
		dirs: Arc<Dirs>,
		thread_pool: PriorityThreadPool,
		path: PathBuf,
	) -> Self {
		let dir_reader = DirReader::new(path);
		dir_reader.set_visitor(DirReaderVisitor {
			ctx: cc.egui_ctx.clone(),
		});
		dir_reader.add_allowed_extensions(["jpg", "jpeg", "png", "gif", "webp", "webm", "mkv", "mp4"]);

		Self {
			dirs,
			thread_pool,
			dir_reader,
			cur_player: None,
			next_frame_idx: 0,
			pan_zoom: PanZoom {
				offset: egui::Vec2::ZERO,
				zoom:   0.0,
			},
			resized_image: false,
			view_mode: ViewMode::FitWindow,
			display_mode: DisplayMode::Image,
			loaded_entries: IndexSet::new(),
			max_loaded_entries: 5,
			shortcuts: Shortcuts::default(),
			entries_per_row: 4,
		}
	}

	fn reset_on_change_entry(&mut self, new_entry: &CurEntry) {
		self.pan_zoom = PanZoom {
			offset: egui::Vec2::ZERO,
			zoom:   0.0,
		};
		self.resized_image = false;

		if self.loaded_entries.len() > self.max_loaded_entries {
			// Try to get the indices or all entries (including the new)
			let idxs: Option<_> = try {
				let new_idx = new_entry.idx?;

				// TODO: Don't ignore errors here?
				// TODO: This could be somewhat expensive, can we
				//       cache this somehow?
				let entry_idxs = self
					.loaded_entries
					.iter()
					.map(|entry| self.dir_reader.idx_of(entry).ok().flatten())
					.collect::<Option<Vec<_>>>()?;

				(new_idx, entry_idxs)
			};

			// Then choose which entry to remove
			let to_remove_loaded_idx = match idxs {
				// If we have all the indices, select the furthest one
				Some((new_idx, entry_idxs)) => entry_idxs
					.iter()
					.position_max_by_key(|entry_idx| entry_idx.abs_diff(new_idx))
					.expect("Just checked it wasn't empty"),

				// If we don't have all the indices, there's a re-order
				// happening right now, so we just conservatively remove
				// the oldest item
				// Note: The current entry cannot be the oldest because we
				//       just inserted, so this is guaranteed to not be our
				//       current entry
				None => 0,
			};

			// Finally remove it from the loaded entries and remove it's texture
			let entry = self
				.loaded_entries
				.shift_remove_index(to_remove_loaded_idx)
				.expect("Just checked it wasn't empty");
			entry.remove_texture();
		}
	}

	/// Formats the title
	fn title(&self, cur_entry: &CurEntry) -> String {
		let mut title = format!(
			"{}/{}: {}",
			std::fmt::from_fn(|f| match cur_entry.idx {
				Some(idx) => write!(f, "{}", idx + 1),
				None => write!(f, "?"),
			}),
			self.dir_reader.len(),
			cur_entry.path().file_name().expect("Entry had no file name").display()
		);

		if let Some(img) = cur_entry.image_details() {
			match img {
				ImageDetails::Image { size } => {
					write_str!(title, " {}x{}", size.x, size.y);
				},
				ImageDetails::Video { size, duration } => {
					write_str!(title, " {}x{} ({duration:.2?})", size.x, size.y);
				},
			}
		}

		match cur_entry.try_size(&self.thread_pool) {
			Ok(Some(size)) => write_str!(title, " {}", humansize::format_size(size, humansize::BINARY)),
			Ok(None) => (),
			Err(err) => {
				tracing::warn!("Unable to load size {:?}, removing: {err:?}", cur_entry.path());
				self.dir_reader.cur_entry_remove();
			},
		}

		title
	}

	/// Draws the info window
	fn draw_info_window(&self, ctx: &egui::Context, cur_entry: Option<&CurEntry>) {
		egui::Window::new("Path")
			.title_bar(false)
			.resizable(false)
			.frame(egui::Frame {
				inner_margin: egui::Margin::symmetric(1, 1),
				fill: egui::Color32::from_rgba_premultiplied(0, 0, 0, 128),
				..egui::Frame::NONE
			})
			.anchor(egui::Align2::LEFT_TOP, egui::Vec2::ZERO)
			.default_width(ctx.available_rect().width())
			.show(ctx, |ui| {
				ui.style_mut().visuals.override_text_color = Some(egui::Color32::WHITE);

				let mut info = String::new();
				if let Some(entry) = cur_entry {
					write_str!(info, "{}\n", self.title(entry));
					let view_mode = match self.view_mode {
						ViewMode::FitWindow => "Fit window",
						ViewMode::FitWidth => "Fit width",
						ViewMode::ActualSize => "Actual size",
					};
					write_str!(info, "View mode: {view_mode}\n");
				}

				let sort_order = self.dir_reader.sort_order();
				write_str!(info, "Sort order: {}", self::sort_order_name(sort_order));

				if let Some(sort_progress) = self.dir_reader.sort_progress() {
					write_str!(info, " Sorting {}/{}", sort_progress.sorted, sort_progress.total);
				}

				ui.label(info);
			});
	}

	/// Draws an image over the whole screen
	fn draw_image(
		ui: &mut egui::Ui,
		image: egui::Image,
		image_size: egui::Vec2,
		window_size: egui::Vec2,
		pan_zoom: &mut PanZoom,
		view_mode: ViewMode,
		window_response: &egui::Response,
	) -> egui::Response {
		let window_as = window_size.y / window_size.x;
		let image_as = image_size.y / image_size.x;

		let width_ratio = window_size.x / image_size.x;
		let height_ratio = window_size.y / image_size.y;

		let x_ratio = width_ratio / height_ratio;
		let y_ratio = height_ratio / width_ratio;

		// Calculate where to position the image on screen
		let (ui_pos, ui_size) = match view_mode {
			ViewMode::FitWindow => {
				let ui_pos = match window_as > image_as {
					true => egui::pos2(0.0, window_size.y * (y_ratio - 1.0) / (2.0 * y_ratio)),
					false => egui::pos2(window_size.x * (x_ratio - 1.0) / (2.0 * x_ratio), 0.0),
				};

				let ui_size = match window_as > image_as {
					true => egui::vec2(window_size.x, window_size.y / y_ratio),
					false => egui::vec2(window_size.x / x_ratio, window_size.y),
				};

				(ui_pos, ui_size)
			},
			ViewMode::FitWidth => {
				let ui_pos = match window_as > image_as {
					true => egui::pos2(0.0, window_size.y * (y_ratio - 1.0) / (2.0 * y_ratio)),
					false => egui::Pos2::ZERO,
				};

				let ui_size = match window_as > image_as {
					true => egui::vec2(window_size.x, window_size.y / y_ratio),
					false => egui::vec2(window_size.x, window_size.y * x_ratio),
				};

				(ui_pos, ui_size)
			},
			ViewMode::ActualSize => {
				let ui_pos = (window_size / 2.0 - image_size / 2.0).max(egui::Vec2::ZERO).to_pos2();
				let ui_size = image_size;

				(ui_pos, ui_size)
			},
		};

		// Calculate our zoom, window/ui and uv scale
		//
		// # Zoom scale
		// As the name implies, it's used to scale the uvs (and only them)
		// to get to the part we're actually looking at
		//
		// # Window/Ui
		// This is the relation between the window and the ui we emitted.
		// Since we clamp it to a maximum of 1, this is only different from
		// 1 when in fit-window mode in fullscreen, since the image won't cover
		// up the entire screen.
		// It's used to scale both the ui and uvs at the end to avoid having the
		// ui be bigger than the actual window size. This is technically not
		// necessary, since `egui` handles bigger-than-window widgets fine, but
		// it makes the reasoning easier.
		//
		// # Uv
		// This also relates to the fit-window mode, where we make the ui larger
		// as long as there's still window space and the uvs aren't covering
		// the whole image
		// TODO: Choose cheaper functions than exponentials and logarithms?
		fn zoom_to_scale(zoom: f32) -> f32 {
			(-zoom / 500.0).exp()
		}
		fn scale_to_zoom(scale: f32) -> f32 {
			-scale.ln() * 500.0
		}
		let mut zoom_scale = zoom_to_scale(pan_zoom.zoom);

		let window_ui_scale = (window_size / ui_size).min(egui::Vec2::ONE);
		// Note: Mutable because we need to update it when zooming in later on
		let mut uv_scale = (window_size / ui_size)
			.min(egui::Vec2::ONE / zoom_scale)
			.max(egui::Vec2::ONE);

		// Afterwards calculate the ui and uv rects and draw our image on them
		//
		// # Ui
		// The ui rect is the rectangle at which we'll position the image on screen.
		// It will always be smaller or equal to window size.
		//
		// # UV
		// The uv rect is the rectangle we use to crop the image (using uv coordinates,
		// as the name implies). It will always be within `[0, 1] x [0, 1]`.
		let ui_rect = egui::Rect::from_min_size(ui_pos, ui_size)
			.scale_from_center2(uv_scale)
			.scale_from_min2(window_ui_scale);
		let uv_rect = egui::Rect::from_min_size(egui::Pos2::ZERO, egui::Vec2::ONE)
			.translate(pan_zoom.offset / image_size)
			.scale_from_center(zoom_scale)
			.scale_from_center2(uv_scale)
			.scale_from_min2(window_ui_scale);

		let image_response = ui.allocate_rect(ui_rect, egui::Sense::all());
		image.uv(uv_rect).paint_at(ui, ui_rect);

		// When dragging or scrolling, handle panning
		// Note: `translation_delta` uses both scrolling and pan gestures (on mobile, which don't include dragging,
		//       for some reason, which is why we add it separately)
		// TODO: Make this scroll sensitivity configurable
		let drag_delta = image_response.drag_delta() + ui.input(egui::InputState::translation_delta) * 2.0;
		if drag_delta != egui::Vec2::ZERO {
			pan_zoom.offset -= drag_delta * zoom_scale * image_size / ui_size;
		}

		// When hovering (either the image or background), handle zooming
		// TODO: Support 2d zoom?
		let scroll_delta = ui.input(egui::InputState::zoom_delta);
		#[expect(
			clippy::float_cmp,
			reason = "Egui sets it to exactly `1.0`, and either way this is just an optimization to avoid \
			          re-calculating the zoom every frame"
		)]
		if scroll_delta != 1.0 &&
			let Some(cursor_pos) = window_response.union(image_response.clone()).hover_pos()
		{
			// TODO: Make this sensitivity configurable
			pan_zoom.zoom += (scroll_delta - 1.0) * 200.0;

			// Cap our zoom to not view outside the window
			let max_zoom_scale = egui::Vec2::ONE / window_ui_scale;
			let min_zoom = scale_to_zoom(max_zoom_scale.min_elem());
			pan_zoom.zoom = pan_zoom.zoom.max(min_zoom);

			// Then update the zoom scale and uv
			let old_zoom_scale = zoom_scale;
			zoom_scale = zoom_to_scale(pan_zoom.zoom);
			uv_scale = (window_size / ui_size)
				.min(egui::Vec2::ONE / zoom_scale)
				.max(egui::Vec2::ONE);

			// Note: When zooming in with modes other than fit-window, we'll
			//       actually zoom into the middle of the image itself, so
			//       we need to adjust it so we zoom correctly.
			//       We also adjust the zoom so it zooms into where the cursor
			//       is on screen for all view modes.
			pan_zoom.offset += image_size *
				(old_zoom_scale - zoom_scale) *
				((window_ui_scale - egui::Vec2::ONE) / 2.0 + (cursor_pos.to_vec2() - window_size / 2.0) / ui_size);
		}

		// These minimum/maximum positions were calculated by setting `uv_rect.min >= 0.0` and
		// `uv_rect.max <= 1.0` and solving for `pan_zoom.offset` in each one.
		let min_pos = image_size * -(egui::Vec2::ONE - zoom_scale * uv_scale) / 2.0;
		let max_pos = image_size *
			((egui::Vec2::ONE - uv_scale * zoom_scale * window_ui_scale) -
				(egui::Vec2::ONE - zoom_scale * uv_scale) / 2.0);

		pan_zoom.offset = pan_zoom.offset.clamp(min_pos, max_pos);

		image_response
	}

	/// Draws an entry
	fn draw_entry(&mut self, ui: &mut egui::Ui, input: DrawInput) -> DrawOutput {
		let mut output = DrawOutput {
			window_response:  None,
			image_size:       None,
			resize_size:      None,
			remove_cur_entry: false,
		};

		let entry_path = input.entry.path();
		match entry_path.extension().and_then(OsStr::to_str) {
			Some("mkv" | "mp4" | "webm" | "gif") => {
				let player = match &mut self.cur_player {
					Some(player) if player.entry == *input.entry => player,
					_ => {
						let Some(path) = entry_path.to_str() else {
							tracing::warn!("Non-utf8 video paths cannot be played currently");
							output.remove_cur_entry = true;
							return output;
						};
						match egui_video::Player::new(ui.ctx(), &path.to_owned()) {
							Ok(mut player) => {
								player.start();
								output.resize_size = Some(player.size);
								self.cur_player.insert(CurPlayer {
									entry: input.entry.clone().into(),
									player,
								})
							},
							Err(err) => {
								tracing::warn!("{:?}", err.context("Unable to create video player"));
								output.remove_cur_entry = true;
								return output;
							},
						}
					},
				};

				if input.toggle_pause {
					match player.player.player_state.get() {
						egui_video::PlayerState::Paused => player.player.resume(),
						egui_video::PlayerState::Playing => player.player.pause(),
						_ => (),
					}
				}

				output.image_size = Some(player.player.size);
				let image_size = player.player.size;

				let image = player
					.player
					.generate_frame_image(image_size)
					.maintain_aspect_ratio(true)
					.fit_to_exact_size(ui.available_size());

				let window_size = ui.available_size().round_ui();
				output.window_response = Some(Self::draw_image(
					ui,
					image,
					image_size,
					window_size,
					&mut self.pan_zoom,
					self.view_mode,
					input.window_response,
				));

				player.player.render_controls(ui, input.window_response);
				player.player.process_state();

				if !input.entry.has_image_details() {
					let duration_ms = match u64::try_from(player.player.duration_ms) {
						Ok(duration) => duration,
						Err(_) => {
							tracing::warn!("Video duration was negative: {}ms", player.player.duration_ms);
							0
						},
					};

					input.entry.set_image_details(ImageDetails::Video {
						size:     image_size,
						duration: Duration::from_millis(duration_ms),
					});
				}
			},

			_ => {
				// Note: It's important we check the loaded textures *before*
				//       returning, else we could accumulate a bunch of loading
				//       textures
				// Note: It's important we call `entry.texture` before
				//       `entry.remove_texture`, otherwise the texture would be
				//       "leaked" (as-in, it would be outside `loaded_entries`
				//       while being loaded)
				let image_texture = match input.entry.texture(&self.thread_pool, ui.ctx()) {
					Ok(texture) => texture,
					Err(err) => {
						tracing::warn!("Unable to load image {:?}, removing: {err:?}", input.entry.path());
						self.dir_reader.cur_entry_remove();
						return output;
					},
				};

				self.loaded_entries.insert(input.entry.clone().into());
				let Some(image_texture) = image_texture else {
					ui.centered_and_justified(|ui| {
						ui.weak("Loading...");
					});
					return output;
				};

				let image = egui::Image::from_texture(egui::load::SizedTexture::from_handle(&image_texture))
					.show_loading_spinner(false)
					.sense(egui::Sense::click());

				let load_res = image.load_for_size(ui.ctx(), egui::Vec2::INFINITY);
				let image_size = match load_res {
					Ok(tex) => match tex.size() {
						// TODO: We can have a size even while not loaded, should we still continue?
						Some(size) => size,
						None => {
							ui.centered_and_justified(|ui| {
								ui.weak("Loading...");
							});
							return output;
						},
					},
					Err(err) => {
						tracing::warn!("Unable to load image {:?}, removing: {err:?}", input.entry.path());
						self.dir_reader.cur_entry_remove();
						return output;
					},
				};

				output.image_size = Some(image_size);

				// Note: If we're in fullscreen, we shouldn't resize yet.
				//       If we did, the user might notice a flicker, because
				//       our resize size isn't the fullscreen size the user
				//       is using, and resizing won't change that.
				let is_fullscreen = ui.input(|input| input.viewport().fullscreen).unwrap_or(false);
				let window_size = match self.resized_image || is_fullscreen {
					true => ui.available_size().round_ui(),
					false => {
						let Some(monitor_size) = ui.input(|input| input.viewport().monitor_size) else {
							return output;
						};

						let scale = f32::min(monitor_size.x / image_size.x, monitor_size.y / image_size.y);
						let image_size_monitor = (image_size * scale).round_ui();

						output.resize_size = Some(image_size_monitor);
						self.resized_image = true;

						image_size_monitor
					},
				};

				output.window_response = Some(Self::draw_image(
					ui,
					image,
					image_size,
					window_size,
					&mut self.pan_zoom,
					self.view_mode,
					input.window_response,
				));

				if !input.entry.has_image_details() {
					input.entry.set_image_details(ImageDetails::Image { size: image_size });
				}
			},
		}

		output
	}

	fn draw_display_image(&mut self, ctx: &egui::Context) {
		let mut fullscreen = false;
		let mut move_prev = false;
		let mut move_next = false;
		let mut move_first = false;
		let mut move_last = false;
		let mut toggle_pause = false;
		ctx.input_mut(|input| {
			fullscreen = input.pointer.button_double_clicked(egui::PointerButton::Primary);

			move_prev = input.consume_key(egui::Modifiers::NONE, self.shortcuts.prev);
			move_next = input.consume_key(egui::Modifiers::NONE, self.shortcuts.next);

			move_first = input.consume_key(egui::Modifiers::NONE, self.shortcuts.first);
			move_last = input.consume_key(egui::Modifiers::NONE, self.shortcuts.last);

			toggle_pause = input.consume_key(egui::Modifiers::NONE, self.shortcuts.toggle_pause);


			for (&view_mode, &key) in &self.shortcuts.view_modes {
				if input.consume_key(egui::Modifiers::NONE, key) {
					self.view_mode = view_mode;
					self.pan_zoom = PanZoom {
						offset: egui::Vec2::ZERO,
						zoom:   0.0,
					};
				}
			}
		});

		let Some(mut cur_entry) = self.dir_reader.cur_entry() else {
			return;
		};

		if move_prev && let Some(entry) = self.dir_reader.cur_entry_set_prev() {
			cur_entry = entry;
			self.reset_on_change_entry(&cur_entry);
		}
		if move_next && let Some(entry) = self.dir_reader.cur_entry_set_next() {
			cur_entry = entry;
			self.reset_on_change_entry(&cur_entry);
		}
		if move_first && let Some(entry) = self.dir_reader.cur_entry_set_first() {
			cur_entry = entry;
			self.reset_on_change_entry(&cur_entry);
		}
		if move_last && let Some(entry) = self.dir_reader.cur_entry_set_last() {
			cur_entry = entry;
			self.reset_on_change_entry(&cur_entry);
		}

		let cur_entry = cur_entry;
		let cur_entry_path = cur_entry.path();

		self.draw_info_window(ctx, Some(&cur_entry));

		let panel_output = egui::CentralPanel::default().frame(egui::Frame::NONE).show(ctx, |ui| {
			let window_response = ui.interact(
				egui::Rect::from_min_size(egui::Pos2::ZERO, ui.available_size()),
				egui::Id::new("whole-screen"),
				egui::Sense::all(),
			);

			let draw_input = DrawInput {
				toggle_pause,
				entry: &cur_entry,
				window_response: &window_response,
			};
			let mut draw_output = self.draw_entry(ui, draw_input);
			let response = match draw_output.window_response.take() {
				Some(response) => response.union(window_response),
				None => window_response,
			};

			// Merge the window response with the image so we catch events on both
			egui::Popup::context_menu(&response).show(|ui| {
				if ui.button("Open").clicked() &&
					let Err(err) = opener::open(&*cur_entry_path)
				{
					tracing::warn!("Unable to open file {:?}: {:?}", cur_entry_path, AppError::new(&err));
				}

				if ui.button("Open in directory").clicked() &&
					let Err(err) = opener::reveal(&cur_entry_path)
				{
					tracing::warn!("Unable to open file {:?}: {:?}", cur_entry_path, AppError::new(&err));
				}

				ui.menu_button("Sort order", |ui| {
					let cur_sort_order = self.dir_reader.sort_order();
					for &sort_order_kind in SortOrderKind::VARIANTS {
						let sort_order = SortOrder {
							reverse: match cur_sort_order.kind == sort_order_kind {
								true => !cur_sort_order.reverse,
								false => false,
							},
							kind:    sort_order_kind,
						};
						let name = self::sort_order_name(sort_order);

						if ui.button(name).clicked() {
							self.dir_reader.set_sort_order(sort_order);
						}
					}
				});
			});

			draw_output
		});
		let draw_output = panel_output.inner;

		if let Some(size) = draw_output.resize_size {
			tracing::info!("Resizing to {}x{}", size.x, size.y);
			ctx.send_viewport_cmd(egui::ViewportCommand::InnerSize(size));
		}

		// TODO: Don't change the title each frame?
		ctx.send_viewport_cmd(egui::ViewportCommand::Title(self.title(&cur_entry)));

		if draw_output.remove_cur_entry {
			self.dir_reader.cur_entry_remove();
		}

		// TODO: Don't duplicate this by returning the fact that we want to fullscreen instead
		// TODO: Also allow double-clicking an empty part in the display list to fullscreen
		let is_fullscreen = ctx.input(|input| input.viewport().fullscreen).unwrap_or(false);
		if fullscreen {
			ctx.send_viewport_cmd(egui::ViewportCommand::Fullscreen(!is_fullscreen));
		}
	}

	fn draw_display_list(&mut self, ctx: &egui::Context) {
		let mut goto_entry = None;

		egui::TopBottomPanel::top("display-list-controls")
			.show_separator_line(false)
			.frame(egui::Frame::NONE)
			.show(ctx, |ui| {
				self.draw_info_window(ctx, None);

				ui.with_layout(egui::Layout::right_to_left(egui::Align::Min), |ui| {
					egui::Slider::new(&mut self.entries_per_row, 1..=10).ui(ui);
					ui.label("Columns: ");
				});
			});

		egui::CentralPanel::default().frame(egui::Frame::NONE).show(ctx, |ui| {
			let total_entries = self.dir_reader.len();
			let entry_rows = total_entries.div_ceil(self.entries_per_row);
			let image_height = ui.available_width() / self.entries_per_row as f32;

			// TODO: Not have to manually calculate this
			let text_body_height = ui.text_style_height(&egui::TextStyle::Body);
			let row_height = ui.available_width() / self.entries_per_row as f32 +
				text_body_height +
				10.0 * 2.0 + 5.0 * 2.0 +
				1.0 * 2.0;

			egui::ScrollArea::vertical()
				.auto_shrink(false)
				.show_rows(ui, row_height, entry_rows, |ui, rows| {
					let row_width = ui.available_width();
					let cell_width = row_width / self.entries_per_row as f32;
					let cell_height = row_height;

					egui::Grid::new("display-list-entries")
						.num_columns(self.entries_per_row)
						.min_row_height(cell_height)
						.min_col_width(cell_width)
						.max_col_width(cell_width)
						.start_row(rows.start)
						.spacing([0.0, 0.0])
						.show(ui, |ui| {
							for row in rows {
								let idxs =
									(row * self.entries_per_row)..((row + 1) * self.entries_per_row).min(total_entries);
								let Some(entries) = self.dir_reader.entry_range(idxs) else {
									return;
								};

								for entry in entries {
									let hovered_id = egui::Id::new(("display-list-hover", &entry));

									let hovered = ui.data(|data| data.get_temp(hovered_id)).unwrap_or(false);
									let stroke_color = match hovered {
										true => egui::Color32::from_rgba_premultiplied(0xd0, 0xd0, 0xd0, 0xff),
										false => egui::Color32::TRANSPARENT,
									};

									let frame = egui::Frame::NONE
										.inner_margin(5)
										.outer_margin(10)
										.stroke(egui::Stroke::new(1.0, stroke_color))
										.fill(egui::Color32::from_rgba_premultiplied(0x10, 0x10, 0x10, 0xff))
										.corner_radius(2);

									let frame_res = frame.show(ui, |ui| {
										ui.take_available_space();

										let image_width = ui.available_width();
										let image_size = egui::vec2(image_width, image_height);

										ui.vertical_centered_justified(|ui| {
											match entry.thumbnail_texture(&self.thread_pool, ui.ctx(), &self.dirs) {
												Ok(Some(image_texture)) => {
													let image = egui::Image::from_texture(
														egui::load::SizedTexture::from_handle(&image_texture),
													)
													.fit_to_exact_size(image_size)
													.show_loading_spinner(false)
													.sense(egui::Sense::click());

													if ui.add(image).double_clicked() {
														goto_entry = Some(entry.clone());
													}
												},
												Ok(None) => {},
												Err(err) => {
													tracing::warn!(
														"Unable to load image {:?}, removing: {err:?}",
														entry.path()
													);
													self.dir_reader.remove(&entry);
												},
											}

											let file_name_height_id = egui::Id::new(("display-list-hover", &entry));
											let file_name_height =
												ui.data(|data| data.get_temp(file_name_height_id)).unwrap_or(0.0);

											let spacing = ui.available_height() - file_name_height;
											if spacing > 0.0 {
												ui.add_space(spacing);
											}

											if let Ok(file_name) = entry.file_name() {
												let response = ui.label(file_name);
												ui.data_mut(|data| {
													data.insert_temp(file_name_height_id, response.rect.height());
												});
											}
										});
									});

									ui.data_mut(|data| data.insert_temp(hovered_id, frame_res.response.hovered()));
								}
								ui.end_row();
							}
						});
				});
		});

		if let Some(entry) = goto_entry {
			self.dir_reader.cur_entry_set(entry);
			self.display_mode = DisplayMode::Image;
		}
	}
}

impl eframe::App for EguiApp {
	fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
		// For the first 2 frames don't draw anything, because resizes and other
		// window-related things don't work yet
		self.next_frame_idx += 1;
		if self.next_frame_idx <= 2 {
			return;
		}

		let mut fullscreen = false;
		let mut exit_fullscreen_or_quit = false;
		let mut quit = false;
		let mut toggle_display_mode = false;
		let mut set_sort_order = None;
		ctx.input_mut(|input| {
			fullscreen = input.consume_key(egui::Modifiers::NONE, self.shortcuts.fullscreen);

			exit_fullscreen_or_quit = input.consume_key(egui::Modifiers::NONE, self.shortcuts.exit_fullscreen_or_quit);

			quit = input.consume_key(egui::Modifiers::NONE, self.shortcuts.quit);

			toggle_display_mode = input.consume_key(egui::Modifiers::NONE, self.shortcuts.toggle_display_mode);

			for (&kind, &key) in &self.shortcuts.sort {
				if input.consume_key(egui::Modifiers::NONE, key) {
					let mut sort_order = self.dir_reader.sort_order();
					match sort_order.kind == kind {
						true => sort_order.reverse ^= true,
						false => sort_order.kind = kind,
					}

					set_sort_order = Some(sort_order);
				}
			}
		});

		match self.display_mode {
			DisplayMode::Image => self.draw_display_image(ctx),
			DisplayMode::List => self.draw_display_list(ctx),
		}

		if let Some(sort_order) = set_sort_order {
			self.dir_reader.set_sort_order(sort_order);
		}

		let is_fullscreen = ctx.input(|input| input.viewport().fullscreen).unwrap_or(false);
		if fullscreen {
			ctx.send_viewport_cmd(egui::ViewportCommand::Fullscreen(!is_fullscreen));
		}

		if exit_fullscreen_or_quit {
			match is_fullscreen {
				true => ctx.send_viewport_cmd(egui::ViewportCommand::Fullscreen(false)),
				false => ctx.send_viewport_cmd(egui::ViewportCommand::Close),
			}
		}

		if quit {
			ctx.send_viewport_cmd(egui::ViewportCommand::Close);
		}

		if toggle_display_mode {
			self.display_mode = self.display_mode.toggle();
		}
	}
}

/// Returns the name of a sort order
fn sort_order_name(sort_order: SortOrder) -> String {
	let sort_order_kind = match sort_order.kind {
		SortOrderKind::FileName => "File name",
		SortOrderKind::ModificationDate => "Modified date",
		SortOrderKind::Size => "Size",
	};

	match sort_order.reverse {
		true => format!("{sort_order_kind} (Reverse)"),
		false => sort_order_kind.to_owned(),
	}
}

struct DirReaderVisitor {
	ctx: egui::Context,
}

impl dir_reader::Visitor for DirReaderVisitor {
	fn entry_added(&self, _dir_entry: DirEntry) {
		self.ctx.request_repaint();
	}
}

macro write_str(
	$($t:tt)*
) {
	write!( $($t)* ).expect("Writing into strings cannot fail")
}

#[derive(Clone, Copy, Debug)]
struct DrawInput<'a> {
	entry:           &'a CurEntry,
	window_response: &'a egui::Response,
	toggle_pause:    bool,
}

#[derive(Clone, Debug)]
struct DrawOutput {
	window_response:  Option<egui::Response>,
	image_size:       Option<egui::Vec2>,
	resize_size:      Option<egui::Vec2>,
	remove_cur_entry: bool,
}

#[derive(Clone, Copy, Debug)]
struct PanZoom {
	offset: egui::Vec2,
	zoom:   f32,
}
