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
	decl_macro
)]

// Modules
mod args;
mod dir_reader;
mod util;

// Imports
use {
	self::{
		args::Args,
		dir_reader::{CurEntry, DirEntry, DirReader, SortOrder, SortOrderKind, entry::ImageDetails},
		util::{AppError, Pos2Utils},
	},
	app_error::Context,
	clap::Parser,
	egui::emath::GuiRounding,
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
struct EguiApp {
	dir_reader:     DirReader,
	cur_player:     Option<CurPlayer>,
	next_frame_idx: usize,
	image_zoom:     egui::Rect,
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
		dir_reader.add_allowed_extensions(["jpg", "jpeg", "png", "gif", "webp", "webm", "mkv"]);

		Self {
			dir_reader,
			cur_player: None,
			next_frame_idx: 0,
			image_zoom: egui::Rect::ZERO,
			resized_image: false,
			view_mode: ViewMode::FitWindow,
		}
	}

	const fn reset_on_change_entry(&mut self) {
		self.image_zoom = egui::Rect::ZERO;
		self.resized_image = false;
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
				ImageDetails::Video {} => (),
			}
		}

		if let Some(size) = cur_entry.size() {
			write_str!(title, " {}", humansize::format_size(size, humansize::BINARY));
		}

		title
	}

	/// Draws the info window
	fn draw_info_window(&self, ctx: &egui::Context, cur_entry: &CurEntry) {
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

				let mut info = self.title(cur_entry);
				let view_mode = match self.view_mode {
					ViewMode::FitWindow => "Fit window",
					ViewMode::FitWidth => "Fit width",
					ViewMode::ActualSize => "Actual size",
				};
				write_str!(info, "\nView mode: {view_mode}");

				let sort_order = self.dir_reader.sort_order();
				write_str!(info, "\nSort order: {}", self::sort_order_name(sort_order));

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
		image_zoom: &mut egui::Rect,
		view_mode: ViewMode,
		window_response: &egui::Response,
	) -> egui::Response {
		// If our zoom rectangle is uninitialized, initialize it
		if *image_zoom == egui::Rect::ZERO {
			*image_zoom = egui::Rect::from_min_size(egui::Pos2::ZERO, egui::Vec2::ONE);
		}

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

		let ui_pos = ui_pos.round_ui();
		let ui_size = ui_size.round_ui();

		// After obtaining the ui position and size, adjust it using uvs
		// Note: Since the uvs may be negative (corresponding to only showing part of
		//       the texture on-screen), we need to move the ui position around.
		let mut ui_rect = egui::Rect::from_min_size(ui_pos, ui_size);
		let mut uv_rect = *image_zoom;

		if uv_rect.width() < 1.0 || uv_rect.height() < 1.0 {
			let scale = egui::Vec2::ONE / uv_rect.size();
			ui_rect = ui_rect.scale_from_center2(scale);
			uv_rect = uv_rect.scale_from_center2(scale);
		}

		// Draw the image itself at our calculated ui with uvs.
		let image_response = ui.allocate_rect(ui_rect, egui::Sense::all());
		image.uv(uv_rect).paint_at(ui, ui_rect);

		// Handle panning
		if image_response.dragged() {
			let scale = image_zoom.size() / ui_size;
			*image_zoom = image_zoom.translate(-image_response.drag_delta() * scale);
		}

		// Handle zooming if the user is hovering
		// Note: We want to zoom on both the background and the image, so we join them
		let scroll_delta = ui.input(|input| input.smooth_scroll_delta.y);
		if scroll_delta != 0.0 &&
			let Some(cursor_pos) = window_response.union(image_response.clone()).hover_pos()
		{
			let window_middle = egui::Pos2::ZERO + window_size / 2.0;
			let zoom = 0.001 * scroll_delta * image_zoom.size();
			let offset = zoom * 2.0 * (cursor_pos - window_middle) / ui_size;

			*image_zoom = image_zoom.shrink2(zoom).translate(offset);
		}

		// Normalize the image zoom to avoid infinite zoom out and panning out
		// of the sides of the image.
		if image_zoom.width() > 1.0 || image_zoom.height() > 1.0 {
			let scale = egui::Vec2::ONE / image_zoom.size();
			*image_zoom = image_zoom.scale_from_center2(scale);
		}

		let mut offset = egui::Vec2::ZERO;

		let min_pos = ui_pos.div_vec2(window_size);
		if image_zoom.min.x < min_pos.x || image_zoom.min.y < min_pos.y {
			offset += (min_pos - image_zoom.min).max(egui::Vec2::ZERO);
		}

		let max_pos = (ui_pos + ui_size).div_vec2(window_size);
		if image_zoom.max.x > max_pos.x || image_zoom.max.y > max_pos.y {
			offset += (max_pos - image_zoom.max).min(egui::Vec2::ZERO);
		}

		let width = image_zoom.width();
		if min_pos.x + width >= max_pos.x {
			offset.x = 0.0;

			let pos = f32::midpoint(min_pos.x, max_pos.x);
			image_zoom.min.x = pos - width / 2.0;
			image_zoom.max.x = pos + width / 2.0;
		}

		let height = image_zoom.height();
		if min_pos.y + height >= max_pos.y {
			offset.y = 0.0;

			let pos = f32::midpoint(min_pos.y, max_pos.y);
			image_zoom.min.y = pos - height / 2.0;
			image_zoom.max.y = pos + height / 2.0;
		}

		*image_zoom = image_zoom.translate(offset);

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
			Some("mkv" | "webm" | "gif") => {
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
					&mut self.image_zoom,
					self.view_mode,
					input.window_response,
				));

				player.player.render_controls(ui, input.window_response);
				player.player.process_state();

				if !input.entry.has_image_details() {
					input.entry.set_image_details(ImageDetails::Video {});
				}
			},

			_ => {
				let image = egui::Image::from_uri(format!("file://{}", entry_path.display()))
					.show_loading_spinner(false)
					.sense(egui::Sense::click());

				let Some(image_size) = image.load_and_calc_size(ui, egui::Vec2::INFINITY) else {
					ui.centered_and_justified(|ui| {
						ui.weak("Loading...");
					});

					return output;
				};

				output.image_size = Some(image_size);

				let window_size = match self.resized_image {
					true => ui.available_size().round_ui(),
					false => {
						self.resized_image = true;

						let Some(monitor_size) = ui.input(|input| input.viewport().monitor_size) else {
							return output;
						};
						let image_size_monitor = image.calc_size(monitor_size, Some(image_size)).round_ui();


						output.resize_size = Some(image_size_monitor);
						image_size_monitor
					},
				};

				output.window_response = Some(Self::draw_image(
					ui,
					image,
					image_size,
					window_size,
					&mut self.image_zoom,
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
					SortOrderKind::Size => egui::Key::F3,
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
					self.image_zoom = egui::Rect::ZERO;
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

		let cur_entry = cur_entry;
		let cur_entry_path = cur_entry.path();

		self.draw_info_window(ctx, &cur_entry);

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

			draw_output
		});
		let draw_output = panel_output.inner;

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

		// TODO: Don't change the title each frame?
		ctx.send_viewport_cmd(egui::ViewportCommand::Title(self.title(&cur_entry)));

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
