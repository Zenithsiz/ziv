//! Zenithsiz's image viewer

// Features
#![feature(
	proc_macro_hygiene,
	once_cell_try,
	stmt_expr_attributes,
	try_blocks,
	arbitrary_self_types
)]

// Modules
mod args;
mod dir_reader;
mod util;

// Imports
use {
	self::{
		args::Args,
		dir_reader::{DirEntry, DirReader, SortOrder, SortOrderKind},
		util::AppError,
	},
	app_error::Context,
	clap::Parser,
	eframe::egui::{self},
	std::{ffi::OsStr, fmt::Write, path::PathBuf, process::ExitCode},
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

	// Set logger file from arguments
	logger.set_file(args.log_file.as_deref());

	let path = match args.path {
		Some(path) => path,
		None => std::env::current_dir().context("Unable to get current directory")?,
	};
	let native_options = eframe::NativeOptions::default();
	eframe::run_native(
		"ziv",
		native_options,
		Box::new(|cc| {
			let app = EguiApp::new(cc, path);
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

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
enum ViewMode {
	FitWindow,
	FitWidth,
	ActualSize,
}

#[derive(Debug)]
struct PanZoom {
	offset: egui::Vec2,
	zoom:   f32,
}

#[derive(Debug)]
struct EguiApp {
	dir_reader:     DirReader,
	cur_player:     Option<CurPlayer>,
	next_frame_idx: usize,
	pan_zoom:       PanZoom,
	resized_image:  bool,
	view_mode:      ViewMode,
}

impl EguiApp {
	/// Creates a new app
	pub fn new(cc: &eframe::CreationContext<'_>, path: PathBuf) -> Self {
		egui_extras::install_image_loaders(&cc.egui_ctx);
		let dir_reader = DirReader::new(path);
		dir_reader.set_visitor(DirReaderVisitor {
			ctx: cc.egui_ctx.clone(),
		});

		Self {
			dir_reader,
			cur_player: None,
			next_frame_idx: 0,
			pan_zoom: PanZoom {
				offset: egui::Vec2::ZERO,
				zoom:   0.0,
			},
			resized_image: false,
			view_mode: ViewMode::FitWindow,
		}
	}

	const fn reset_on_change_entry(&mut self) {
		self.pan_zoom = PanZoom {
			offset: egui::Vec2::ZERO,
			zoom:   0.0,
		};
		self.resized_image = false;
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

		let mut move_prev = false;
		let mut move_next = false;
		let mut move_first = false;
		let mut move_last = false;
		let mut fullscreen = false;
		let mut escape = false;
		let mut toggle_pause = false;
		let mut set_sort_order = None;
		let mut new_view_mode = false;
		ctx.input_mut(|input| {
			move_prev = input.consume_key(egui::Modifiers::NONE, egui::Key::ArrowLeft);
			move_next = input.consume_key(egui::Modifiers::NONE, egui::Key::ArrowRight);

			move_first = input.consume_key(egui::Modifiers::NONE, egui::Key::Home);
			move_last = input.consume_key(egui::Modifiers::NONE, egui::Key::End);

			fullscreen = input.consume_key(egui::Modifiers::NONE, egui::Key::F);
			escape = input.consume_key(egui::Modifiers::NONE, egui::Key::Escape);

			toggle_pause = input.consume_key(egui::Modifiers::NONE, egui::Key::Space);

			let sort_orders = SortOrder::KINDS.map(|kind| {
				let key = match kind {
					SortOrderKind::FileName => egui::Key::F1,
					SortOrderKind::ModificationDate => egui::Key::F2,
				};

				(key, kind)
			});

			for (key, kind) in sort_orders {
				if input.consume_key(egui::Modifiers::NONE, key) {
					let mut sort_order = self.dir_reader.sort_order();
					match sort_order.kind == kind {
						true => sort_order.reverse ^= true,
						false => sort_order.kind = kind,
					}

					set_sort_order = Some(sort_order);
				}
			}

			for (key, view_mode) in [
				(egui::Key::Num1, ViewMode::FitWindow),
				(egui::Key::Num2, ViewMode::FitWidth),
				(egui::Key::Num3, ViewMode::ActualSize),
			] {
				if input.consume_key(egui::Modifiers::NONE, key) {
					self.view_mode = view_mode;
					new_view_mode = true;
				}
			}
		});

		if let Some(sort_order) = set_sort_order {
			self.dir_reader.set_sort_order(sort_order);
		}

		let Some(mut cur_entry) = self.dir_reader.cur_entry() else {
			return;
		};

		if move_prev && let Some(entry) = self.dir_reader.cur_entry_set_prev() {
			cur_entry = entry;
			self.reset_on_change_entry();
		}
		if move_next && let Some(entry) = self.dir_reader.cur_entry_set_next() {
			cur_entry = entry;
			self.reset_on_change_entry();
		}
		if move_first && let Some(entry) = self.dir_reader.cur_entry_set_first() {
			cur_entry = entry;
			self.reset_on_change_entry();
		}
		if move_last && let Some(entry) = self.dir_reader.cur_entry_set_last() {
			cur_entry = entry;
			self.reset_on_change_entry();
		}

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

				ui.label(format!(
					"{}/{}: {}",
					std::fmt::from_fn(|f| match cur_entry.idx {
						Some(idx) => write!(f, "{}", idx + 1),
						None => write!(f, "?"),
					}),
					self.dir_reader.len(),
					cur_entry.path().file_name().expect("Entry had no file name").display()
				));

				let view_mode = match self.view_mode {
					ViewMode::FitWindow => "Fit window",
					ViewMode::FitWidth => "Fit width",
					ViewMode::ActualSize => "Actual size",
				};
				ui.label(format!("View mode: {view_mode}"));

				let sort_order = self.dir_reader.sort_order();
				ui.label(format!("Sort order: {}", self::sort_order_name(sort_order)));

				if let Some(sort_progress) = self.dir_reader.sort_progress() {
					ui.label(format!("Sorting {}/{}", sort_progress.sorted, sort_progress.total));
				}
			});

		struct DrawOutput {
			image_size:       Option<egui::Vec2>,
			resize_size:      Option<egui::Vec2>,
			remove_cur_entry: bool,
		}
		let mut draw_output = DrawOutput {
			image_size:       None,
			resize_size:      None,
			remove_cur_entry: false,
		};
		let response = egui::CentralPanel::default().frame(egui::Frame::NONE).show(ctx, |ui| {
			match cur_entry.path().extension().and_then(OsStr::to_str) {
				Some("mkv" | "gif") => {
					let player = match &mut self.cur_player {
						Some(player) if player.entry == cur_entry.entry => player,
						_ => {
							let Some(path) = cur_entry.path().to_str() else {
								tracing::warn!("Non-utf8 video paths cannot be played currently");
								draw_output.remove_cur_entry = true;
								return None;
							};
							match egui_video::Player::new(ctx, &path.to_owned()) {
								Ok(mut player) => {
									player.start();
									draw_output.resize_size = Some(player.size);
									self.cur_player.insert(CurPlayer {
										entry: cur_entry.entry.clone(),
										player,
									})
								},
								Err(err) => {
									tracing::warn!("{:?}", err.context("Unable to create video player"));
									draw_output.remove_cur_entry = true;
									return None;
								},
							}
						},
					};

					if toggle_pause {
						match player.player.player_state.get() {
							egui_video::PlayerState::Paused => player.player.resume(),
							egui_video::PlayerState::Playing => player.player.pause(),
							_ => (),
						}
					}

					draw_output.image_size = Some(player.player.size);
					let response = ui
						.centered_and_justified(|ui| {
							let image_size = player.player.size;

							let image = player
								.player
								.generate_frame_image(image_size)
								.maintain_aspect_ratio(true)
								.fit_to_exact_size(ui.available_size());

							// TODO: Allow zooming video in once the controls don't mess up
							let frame_response = ui.add(image);
							player.player.render_controls(ui, &frame_response);
							player.player.process_state();

							frame_response
						})
						.inner;

					Some(response)
				},

				_ => {
					let image = egui::Image::from_uri(format!("file://{}", cur_entry.path().display()))
						.show_loading_spinner(false)
						.sense(egui::Sense::click());

					let window_size = ui.available_size();
					let Some(image_size) = image.load_and_calc_size(ui, egui::Vec2::INFINITY) else {
						ui.centered_and_justified(|ui| {
							ui.weak("Loading...");
						});

						return None;
					};

					draw_output.image_size = Some(image_size);

					if !self.resized_image {
						self.resized_image = true;

						let monitor_size = ui.input(|input| input.viewport().monitor_size)?;
						let image_size_monitor = image.calc_size(monitor_size, Some(image_size));


						draw_output.resize_size = Some(image_size_monitor);
					}

					if new_view_mode {
						self.pan_zoom.offset = egui::Vec2::ZERO;
						self.pan_zoom.zoom = 0.0;
					}

					let window_as = window_size.y / window_size.x;
					let image_as = image_size.y / image_size.x;

					let width_ratio = window_size.x / image_size.x;
					let height_ratio = window_size.y / image_size.y;

					let x_ratio = width_ratio / height_ratio;
					let y_ratio = height_ratio / width_ratio;

					let (pos, size) = match self.view_mode {
						ViewMode::FitWindow => {
							let pos = match window_as > image_as {
								true => egui::pos2(0.0, window_size.y * (y_ratio - 1.0) / (2.0 * y_ratio)),
								false => egui::pos2(window_size.x * (x_ratio - 1.0) / (2.0 * x_ratio), 0.0),
							};

							let size = match window_as > image_as {
								true => egui::vec2(window_size.x, window_size.y / y_ratio),
								false => egui::vec2(window_size.x / x_ratio, window_size.y),
							};

							(pos, size)
						},
						ViewMode::FitWidth => {
							let pos = match window_as > image_as {
								true => egui::pos2(0.0, window_size.y * (y_ratio - 1.0) / (2.0 * y_ratio)),
								false => egui::Pos2::ZERO,
							};

							let size = match window_as > image_as {
								true => egui::vec2(window_size.x, window_size.y / y_ratio),
								false => egui::vec2(window_size.x, window_size.y * x_ratio),
							};

							(pos, size)
						},
						ViewMode::ActualSize => {
							let pos_x = match window_size.x > image_size.x {
								true => window_size.x / 2.0 - image_size.x / 2.0,
								false => 0.0,
							};

							let pos_y = match window_size.y > image_size.y {
								true => window_size.y / 2.0 - image_size.y / 2.0,
								false => 0.0,
							};

							let pos = egui::pos2(pos_x, pos_y);
							let size = image_size;

							(pos, size)
						},
					};
					let orig_pos = pos;
					let orig_size = size;

					fn zoom_to_scale(zoom: f32) -> f32 {
						(zoom / 200.0).exp()
					}
					fn scale_to_zoom(scale: f32) -> f32 {
						scale.ln() * 200.0
					}

					// TODO: Zoom in on where the mouse is?
					let scale = zoom_to_scale(self.pan_zoom.zoom);
					let pos = pos + self.pan_zoom.offset;
					let pos = pos - (window_size / 2.0);
					let pos = pos * scale;
					let pos = pos + (window_size / 2.0);
					let size = size * scale;

					let rect = egui::Rect {
						min: pos,
						max: pos + size,
					};

					let response = ui.allocate_rect(rect, egui::Sense::all());

					// TODO: We should allow moving on the background, but not in sub-menus,
					//       somehow
					ui.input(|input| {
						if input.pointer.any_down() {
							self.pan_zoom.offset += input.pointer.delta() / scale;
						}

						if input.smooth_scroll_delta.y != 0.0 {
							self.pan_zoom.zoom += input.smooth_scroll_delta.y;
						}

						let min_scale = window_size / orig_size;
						let min_scale = match self.view_mode {
							ViewMode::FitWindow => f32::min(min_scale.x, min_scale.y),
							ViewMode::FitWidth => f32::max(min_scale.x, min_scale.y),
							ViewMode::ActualSize =>
								match (window_size.x > image_size.x, window_size.y > image_size.y) {
									(true, true) => 1.0,
									(true, false) => min_scale.y,
									(false, true) => min_scale.x,
									(false, false) => f32::max(min_scale.x, min_scale.y),
								},
						};
						// TODO: Perform some better rounding than this?
						let min_zoom = (scale_to_zoom(min_scale) * 100.0).ceil() / 100.0;
						self.pan_zoom.zoom = self.pan_zoom.zoom.max(min_zoom);

						let scale = zoom_to_scale(self.pan_zoom.zoom);

						let min_offset = (window_size - window_size / 2.0) / scale + window_size / 2.0 -
							orig_pos.to_vec2() - orig_size;
						let max_offset = (-window_size / 2.0) / scale + window_size / 2.0 - orig_pos.to_vec2();

						match max_offset.x >= min_offset.x {
							true => self.pan_zoom.offset.x = self.pan_zoom.offset.x.clamp(min_offset.x, max_offset.x),
							false => self.pan_zoom.offset.x = 0.0,
						}

						match max_offset.y >= min_offset.y {
							true => self.pan_zoom.offset.y = self.pan_zoom.offset.y.clamp(min_offset.y, max_offset.y),
							false => self.pan_zoom.offset.y = 0.0,
						}
					});

					image.paint_at(ui, rect);

					Some(response)
				},
			}
		});

		if let Some(response) = response.inner {
			egui::Popup::context_menu(&response).show(|ui| {
				if ui.button("Open").clicked() &&
					let Err(err) = opener::open(cur_entry.path())
				{
					tracing::warn!("Unable to open file {:?}: {:?}", cur_entry.path(), AppError::new(&err));
				}

				if ui.button("Open in directory").clicked() &&
					let Err(err) = opener::reveal(cur_entry.path())
				{
					tracing::warn!("Unable to open file {:?}: {:?}", cur_entry.path(), AppError::new(&err));
				}

				ui.menu_button("Sort order", |ui| {
					let cur_sort_order = self.dir_reader.sort_order();
					for sort_order_kind in SortOrder::KINDS {
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
		}

		if let Some(size) = draw_output.resize_size {
			tracing::info!("Resizing to {}x{}", size.x, size.y);
			ctx.send_viewport_cmd(egui::ViewportCommand::InnerSize(size));
		}

		let is_fullscreen = ctx.input(|input| input.viewport().fullscreen).unwrap_or(false);
		if fullscreen {
			ctx.send_viewport_cmd(egui::ViewportCommand::Fullscreen(!is_fullscreen));
		}

		if escape {
			match is_fullscreen {
				true => ctx.send_viewport_cmd(egui::ViewportCommand::Fullscreen(false)),
				false => ctx.send_viewport_cmd(egui::ViewportCommand::Close),
			}
		}

		{
			let mut title = format!(
				"{}/{}: {}",
				std::fmt::from_fn(|f| match cur_entry.idx {
					Some(idx) => write!(f, "{}", idx + 1),
					None => write!(f, "?"),
				}),
				self.dir_reader.len(),
				cur_entry.path().file_name().expect("Entry had no file name").display()
			);
			if let Some(size) = draw_output.image_size {
				write!(title, " {}x{}", size.x, size.y).expect("Writing to strings never fails");
			}
			ctx.send_viewport_cmd(egui::ViewportCommand::Title(title));
		}

		if draw_output.remove_cur_entry {
			self.dir_reader.cur_entry_remove();
		}
	}
}

/// Returns the name of a sort order
fn sort_order_name(sort_order: SortOrder) -> String {
	let sort_order_kind = match sort_order.kind {
		SortOrderKind::FileName => "File name",
		SortOrderKind::ModificationDate => "Modified date",
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
