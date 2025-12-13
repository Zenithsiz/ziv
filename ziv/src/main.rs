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
	yeet_expr,
	impl_trait_in_assoc_type,
	macro_derive,
	yield_expr,
	gen_blocks,
	array_windows,
	thread_sleep_until,
	try_trait_v2,
	try_trait_v2_residual,
	never_type,
	string_replace_in_place,
	formatting_options,
	if_let_guard,
	try_trait_v2_yeet,
	vec_peek_mut
)]

// Modules
mod args;
mod config;
mod dir_reader;
mod dirs;
mod shortcut;
mod util;

// Imports
use {
	self::{
		args::Args,
		config::Config,
		dir_reader::{
			CurEntry,
			DirEntry,
			DirReader,
			SortOrder,
			SortOrderKind,
			entry::{EntryData, EntrySource, EntryThumbnail, image::EntryImageTexture, video::PlayingStatus},
		},
		dirs::Dirs,
		shortcut::{ShortcutKey, Shortcuts, eguiInputStateExt},
		util::{AppError, PriorityThreadPool, RectUtils},
	},
	app_error::Context,
	clap::Parser,
	core::{
		mem,
		ops::{FromResidual, Yeet},
		time::Duration,
	},
	egui::{
		Widget,
		emath::{self, GuiRounding},
	},
	indexmap::IndexSet,
	itertools::Itertools,
	std::{
		fmt::Write,
		fs,
		path::{Path, PathBuf},
		process::ExitCode,
		sync::{Arc, mpsc},
		vec,
	},
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
	let logger = Logger::builder()
		.stderr(std::io::stderr)
		.stderr_filter_default("info")
		.file_filter_default("debug")
		.build();

	// Parse arguments
	let args = Args::parse();
	tracing::debug!(?args, "Arguments");

	let dirs = Dirs::new().context("Unable to determine directories")?;
	let dirs = Arc::new(dirs);

	// Get configuration
	let config_path = args.config_file.as_deref().unwrap_or_else(|| dirs.config()).to_owned();
	let config = util::config::get_or_create_with(&config_path, Config::default);
	tracing::debug!(?config, "Configuration");

	// Set logger file from arguments
	logger.set_file(args.log_file.as_deref());

	let thread_pool = PriorityThreadPool::new().context("Unable to create thread pool")?;

	ffmpeg_next::init().context("Unable to initialize ffmpeg")?;

	let path = match args.path {
		Some(path) => path,
		None => std::env::current_dir().context("Unable to get current directory")?,
	};
	let native_options = eframe::NativeOptions {
		viewport: egui::ViewportBuilder::default().with_title("ziv").with_app_id("ziv"),
		..eframe::NativeOptions::default()
	};
	eframe::run_native(
		"ziv",
		native_options,
		Box::new(|cc| {
			let app =
				EguiApp::new(cc, config_path, config, dirs, thread_pool, path).map_err(AppError::into_std_error)?;
			Ok(Box::new(app))
		}),
	)
	.context("Unable to create window")?;

	Ok(())
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash, Debug)]
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
struct Controls {
	zoom_sensitivity:         f32,
	scroll_sensitivity:       f32,
	keyboard_pan_sensitivity: f32,
	keyboard_pan_smooth:      f32,
}

// TODO: This is a big mess, we need to organize it better
//       and rename things to not be as confusing.
#[derive(Debug)]
struct EguiApp {
	config_path:              PathBuf,
	_dirs:                    Arc<Dirs>,
	thread_pool:              PriorityThreadPool,
	dir_reader:               DirReader,
	next_frame_idx:           usize,
	pan_zoom:                 PanZoom,
	resized_image:            bool,
	view_mode:                ViewMode,
	display_mode:             DisplayMode,
	shortcuts:                Shortcuts,
	entries_per_row:          usize,
	thumbnails_dir:           ThumbnailsDir,
	controls:                 Controls,
	vertical_pan_smooth:      f32,
	settings_is_open:         bool,
	settings_tab:             SettingsTab,
	settings_waiting_for_key: Option<ShortcutKeyIdent>,
	prev_valid_entry:         Option<DirEntry>,

	preload_prev: usize,
	preload_next: usize,

	/// Entries we're loaded
	// TODO: We need a better solution than this
	loaded_entries: IndexSet<DirEntry>,
	max_loaded_entries: usize,

	new_entry_rx:    mpsc::Receiver<DirEntry>,
	loading_entries: Vec<DirEntry>,
}

impl EguiApp {
	/// Creates a new app
	pub fn new(
		cc: &eframe::CreationContext<'_>,
		config_path: PathBuf,
		config: Config,
		dirs: Arc<Dirs>,
		thread_pool: PriorityThreadPool,
		path: PathBuf,
	) -> Result<Self, AppError> {
		let dir_reader = DirReader::new(path).context("Unable to create directory reader")?;
		let (new_entry_tx, new_entry_rx) = mpsc::channel();
		dir_reader.set_visitor(DirReaderVisitor {
			entry_tx: new_entry_tx,
			ctx:      cc.egui_ctx.clone(),
		});

		// Create and canonicalize the thumbnail path
		let thumbnails_dir = match config.thumbnails_cache {
			Some(dir) => ThumbnailsDir::Specified(Arc::from(dir)),
			None => ThumbnailsDir::Auto(Arc::clone(dirs.thumbnails())),
		};
		fs::create_dir_all(thumbnails_dir.path()).context("Unable to create thumbnails directory")?;
		let path = thumbnails_dir
			.path()
			.canonicalize()
			.context("Unable to canonicalize thumbnails directory")?;
		let thumbnails_dir = match thumbnails_dir {
			ThumbnailsDir::Auto(_) => ThumbnailsDir::Auto(Arc::from(path)),
			ThumbnailsDir::Specified(_) => ThumbnailsDir::Specified(Arc::from(path)),
		};

		Ok(Self {
			config_path,
			_dirs: dirs,
			thread_pool,
			dir_reader,
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
			thumbnails_dir,
			shortcuts: config.shortcuts,
			controls: Controls {
				zoom_sensitivity:         config.controls.zoom_sensitivity,
				scroll_sensitivity:       config.controls.scroll_sensitivity,
				keyboard_pan_sensitivity: config.controls.keyboard_pan_sensitivity,
				keyboard_pan_smooth:      config.controls.keyboard_pan_smooth,
			},
			vertical_pan_smooth: 0.0,
			preload_prev: config.preload[0],
			preload_next: config.preload[1],
			settings_is_open: false,
			settings_tab: SettingsTab::General,
			settings_waiting_for_key: None,
			prev_valid_entry: None,
			entries_per_row: 4,
			new_entry_rx,
			loading_entries: vec![],
		})
	}

	/// Saves the configuration
	fn save_config(&self) -> Result<(), AppError> {
		let config = Config {
			thumbnails_cache: match &self.thumbnails_dir {
				ThumbnailsDir::Auto(_path) => None,
				ThumbnailsDir::Specified(path) => Some(path.to_path_buf()),
			},
			preload:          [self.preload_prev, self.preload_next],
			shortcuts:        self.shortcuts.clone(),
			controls:         config::Controls {
				zoom_sensitivity:         self.controls.zoom_sensitivity,
				scroll_sensitivity:       self.controls.scroll_sensitivity,
				keyboard_pan_sensitivity: self.controls.keyboard_pan_sensitivity,
				keyboard_pan_smooth:      self.controls.keyboard_pan_smooth,
			},
		};

		util::config::save(&config, &self.config_path)
	}

	fn remove_entry(&mut self, entry: &DirEntry) {
		// Note: An error here only happens if the entry wasn't already in
		//       the list and it failed to be loaded, so either way it's no
		//       longer on the list and thus we're fine to just log and continue
		if let Err(err) = self.dir_reader.remove(entry) {
			tracing::warn!("Unable to remove entry {:?}: {err:?}", entry.source().name());
		}
		self.loaded_entries.shift_remove(entry);
	}

	// TODO: Not have to pass `&mut Self` here
	fn try_with_entry<T>(&mut self, entry: &DirEntry, f: impl FnOnce(&mut Self, &DirEntry) -> Result<T, AppError>) -> T
	where
		T: FromResidual<Yeet<()>>,
	{
		match f(self, entry) {
			Ok(value) => value,
			Err(err) => {
				tracing::warn!("Unable to load entry {:?}, removing: {err:?}", entry.source().name());
				self.remove_entry(entry);
				do yeet;
			},
		}
	}

	// TODO: Not have to pass `&mut Self` here
	fn with_entry<T>(
		&mut self,
		entry: &DirEntry,
		f: impl FnOnce(&mut Self, &DirEntry) -> Result<T, AppError>,
	) -> Option<T> {
		self.try_with_entry(entry, |this, entry| f(this, entry).map(Some))
	}

	fn reset_on_change_entry(&mut self, prev_entry: &CurEntry, new_entry: &CurEntry) {
		self.pan_zoom = PanZoom {
			offset: egui::Vec2::ZERO,
			zoom:   0.0,
		};
		self.resized_image = false;

		if let Some(EntryData::Video(video)) =
			self.try_with_entry(prev_entry, |_, prev_entry| prev_entry.data_if_exists()) &&
			video.set_offscreen()
		{
			video.pause();
		}

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
					.map(|entry| self.dir_reader.idx_of(entry).ok())
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

			self.with_entry(&entry, |_, entry| {
				let Some(data) = entry.data_if_exists()? else {
					return Ok(());
				};

				match data {
					EntryData::Image(image) => image.unload(),
					EntryData::Video(video) => video.stop(),
					EntryData::Other => (),
				}

				Ok(())
			});
		}
	}

	/// Formats the title
	fn title(&mut self, cur_entry: &CurEntry) -> String {
		let mut title = format!(
			"{}/{}: {}",
			std::fmt::from_fn(|f| match cur_entry.idx {
				Some(idx) => write!(f, "{}", idx + 1),
				None => write!(f, "?"),
			}),
			self.dir_reader.len(),
			cur_entry.file_name().expect("Entry had no file name").display()
		);

		if let Some(data) = self.try_with_entry(cur_entry, |_, cur_entry| cur_entry.data_if_exists()) {
			match data {
				EntryData::Image(image) => {
					if let Some(texture) = self.try_with_entry(cur_entry, |_, _| image.texture()) {
						let [width, height] = texture.size();
						write_str!(title, " {width}x{height}");
					}
					let format = image.format();
					write_str!(title, " ({format:?})");
				},
				EntryData::Video(video) => {
					let size = video.size();
					write_str!(title, " {}x{}", size.x, size.y);
					if let Some(duration) = video.duration() {
						write_str!(title, " ({duration:.2?})");
					}
				},
				EntryData::Other => self.remove_entry(cur_entry),
			}
		}

		if let Some(size) = self.try_with_entry(cur_entry, |this, entry| entry.try_size(&this.thread_pool)) {
			write_str!(title, " {}", humansize::format_size(size, humansize::BINARY));
		}

		title
	}

	/// Draws the info window
	fn draw_info_window(&mut self, ctx: &egui::Context, cur_entry: Option<&CurEntry>) {
		egui::Window::new("info-window")
			.title_bar(false)
			.resizable(false)
			.min_size(egui::Vec2::INFINITY)
			.max_size(egui::Vec2::INFINITY)
			.frame(egui::Frame {
				inner_margin: egui::Margin::symmetric(1, 1),
				fill: egui::Color32::from_rgba_premultiplied(0, 0, 0, 128),
				..egui::Frame::NONE
			})
			.anchor(egui::Align2::LEFT_TOP, egui::Vec2::ZERO)
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

	/// Draws the config window, if open
	fn draw_config_window(&mut self, ctx: &egui::Context) {
		if !self.settings_is_open {
			return;
		}

		struct Output {
			should_close: bool,
		}

		// TODO: Switch to a deferred viewport?
		let id = egui::ViewportId::from_hash_of("config-window");
		let builder = egui::ViewportBuilder::default().with_title("Settings");
		let output = ctx.show_viewport_immediate(id, builder, |ctx, _| {
			let mut output = Output { should_close: false };

			ctx.input(|input| {
				if input.viewport().close_requested() {
					output.should_close = true;
				}
			});

			egui::CentralPanel::default().show(ctx, |ui| {
				ui.horizontal(|ui| {
					for &tab in SettingsTab::VARIANTS {
						ui.selectable_value(&mut self.settings_tab, tab, tab.to_string());
					}
				});
				ui.separator();

				match self.settings_tab {
					SettingsTab::General => {
						ui.collapsing("Preload", |ui| {
							for (name, preload) in
								[("Previous", &mut self.preload_prev), ("Next", &mut self.preload_next)]
							{
								egui::Slider::new(preload, 0..=16)
									.text(name)
									.clamping(egui::SliderClamping::Never)
									.ui(ui);
							}
						});

						ui.collapsing("Controls", |ui| {
							for (name, preload, range) in [
								("Zoom sensitivity", &mut self.controls.zoom_sensitivity, 100.0..=300.0),
								("Scroll sensitivity", &mut self.controls.scroll_sensitivity, 1.0..=3.0),
								(
									"Keyboard pan sensitivity",
									&mut self.controls.keyboard_pan_sensitivity,
									0.0..=1.0,
								),
								("Keyboard pan smooth", &mut self.controls.keyboard_pan_smooth, 0.0..=1.0),
							] {
								egui::Slider::new(preload, range)
									.text(name)
									.clamping(egui::SliderClamping::Never)
									.ui(ui);
							}
						});
					},
					SettingsTab::Shortcuts => {
						if let Some(shortcut_ident) = self.settings_waiting_for_key {
							ctx.input(|input| {
								for event in &input.events {
									let &egui::Event::Key { key, modifiers, .. } = event else {
										continue;
									};

									let shortcut_key = shortcut_ident.get(&mut self.shortcuts);
									shortcut_key.key = key;
									shortcut_key.modifiers = modifiers;
									self.settings_waiting_for_key = None;
									break;
								}
							});
						}

						// TODO: Organize these better?
						for shortcut_ident in ShortcutKeyIdent::variants() {
							let shortcut_key: &mut ShortcutKey = shortcut_ident.get(&mut self.shortcuts);

							ui.horizontal(|ui| {
								ui.label(shortcut_ident.to_string());
								match self.settings_waiting_for_key == Some(shortcut_ident) {
									true =>
										if ui.button("Waiting for key...").clicked() {
											self.settings_waiting_for_key = None;
										},
									false => {
										let mut name = shortcut_key.key.name().to_string();
										if shortcut_key.modifiers.alt {
											name += " (⎇ Alt)";
										}
										if shortcut_key.modifiers.ctrl ||
											(!ctx.os().is_mac() && shortcut_key.modifiers.command)
										{
											name += " (⎈ Ctrl)";
										}
										if shortcut_key.modifiers.shift {
											name += " (⇧ Shift)";
										}
										if shortcut_key.modifiers.mac_cmd ||
											(ctx.os().is_mac() && shortcut_key.modifiers.command)
										{
											name += " (⌘ Cmd)";
										}

										if ui.button(name).clicked() {
											self.settings_waiting_for_key = Some(shortcut_ident);
										}
									},
								}
							});
						}
					},
				}

				ui.with_layout(egui::Layout::bottom_up(egui::Align::Min), |ui| {
					if ui.button("Save").clicked() &&
						let Err(err) = self.save_config()
					{
						tracing::error!("Unable to save configuration file: {err:?}");
					}
					ui.separator();
				});
			});

			output
		});

		if output.should_close {
			self.settings_is_open = false;
		}
	}

	/// Draws an image over the whole screen
	#[expect(clippy::too_many_arguments, reason = "TODO: Bundle them up in a single type")]
	fn draw_image(
		ui: &mut egui::Ui,
		// TODO: Pass parameters as a separate type to avoid confusing both rects.
		draw_image: impl FnOnce(&mut egui::Ui, egui::Rect, egui::Rect),
		image_size: egui::Vec2,
		pan_zoom: &mut PanZoom,
		view_mode: ViewMode,
		window_response: &egui::Response,
		vertical_pan: f32,
		controls: &Controls,
	) -> egui::Response {
		let window_size = ui.available_size().round_ui();
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
		draw_image(ui, ui_rect, uv_rect);


		if window_response.hovered() || image_response.hovered() {
			// When dragging or scrolling, handle panning
			// Note: Dragging must be done with the primary button, otherwise it's zooming.
			// Note: `translation_delta` uses both scrolling and pan gestures (on mobile, which don't include dragging,
			//       for some reason, which is why we add it separately)
			let drag_delta_pointer = match ui.input(|input| input.pointer.primary_down()) {
				true => ui.input(|input| input.pointer.delta()),
				false => egui::vec2(0.0, 0.0),
			};
			let drag_delta = drag_delta_pointer +
				ui.input(egui::InputState::translation_delta) * controls.scroll_sensitivity +
				egui::vec2(0.0, vertical_pan) * window_size * controls.keyboard_pan_sensitivity;
			if drag_delta != egui::Vec2::ZERO {
				pan_zoom.offset -= drag_delta * zoom_scale * image_size / ui_size;
			}

			// When hovering (either the image or background), handle zooming
			// TODO: Support 2d zoom?
			let (zoom_delta, cursor_pos) = ui.input(|input| {
				// Right-click zoom
				// Note: When zooming like this, we want the cursor position to be
				//       at the origin of the drag, not it's current
				let (drag_delta, cursor_pos) = match input.pointer.secondary_down() {
					true => (-input.pointer.delta().y, input.pointer.press_origin()),
					false => (0.0, None),
				};

				// Scrolling zoom
				let scroll_delta = input.zoom_delta() - 1.0;
				let zoom_delta = scroll_delta * controls.zoom_sensitivity;

				// If we still don't have a cursor position, just use the hover position, if any
				let cursor_pos = cursor_pos.or_else(|| input.pointer.hover_pos());

				(zoom_delta + drag_delta, cursor_pos)
			});
			if zoom_delta != 0.0 &&
				let Some(cursor_pos) = cursor_pos
			{
				pan_zoom.zoom += zoom_delta;

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
					((window_ui_scale - egui::Vec2::ONE) / 2.0 +
						(cursor_pos.to_vec2() - window_size / 2.0) / ui_size);
			}
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

	#[expect(clippy::unused_self, reason = "We might use it in the future")]
	fn draw_video_controls(&self, ui: &mut egui::Ui, input: DrawInput<'_>, video: &dir_reader::entry::EntryVideo) {
		let Some(duration) = video.duration() else { return };

		let window_rect = input.window_response.rect;
		let window_pos = window_rect.min;
		let window_size = window_rect.size();

		// Get the animation height scale
		// TODO: We should always be active if hovering over the controls rectangle itself.
		let is_hovering_window = ui.rect_contains_pointer(window_rect);
		let is_pointer_stopped = ui.input(|input| input.pointer.time_since_last_movement()) > 1.0;
		let height_animation_id = egui::Id::new("video-controls-animation");
		let height_animation_time = 0.15;
		let height_animation_active = is_hovering_window && !is_pointer_stopped;
		let height_animation_scale = ui.ctx().animate_bool_with_time_and_easing(
			height_animation_id,
			height_animation_active,
			height_animation_time,
			emath::easing::quadratic_in,
		);
		if height_animation_scale <= 0.0 {
			return;
		}

		// Note: Label height is always constant to avoid the text clipping.
		let label_height = ui.text_style_height(&egui::TextStyle::Body);
		let slider_height = window_size.y * 0.025 * height_animation_scale;

		// Draw the controls background
		let controls_rect = {
			let height = label_height + slider_height;
			let size = egui::vec2(window_size.x, height);
			egui::Rect::from_min_size(window_pos + (window_size - size), size)
		};
		let controls_bg = egui::Color32::from_rgba_unmultiplied(0, 0, 0, 50);
		ui.painter().rect_filled(controls_rect, 0.0, controls_bg);

		// Draw the info
		let label_size = egui::vec2(controls_rect.width(), label_height);
		let label_rect = egui::Rect::from_min_size(controls_rect.min, label_size);
		let label_text = format!(
			"{:.2?} / {:.2?}",
			util::format_duration(video.cur_time()),
			util::format_duration(duration)
		);
		let label = egui::Label::new(label_text);
		{
			let mut ui = ui.new_child(egui::UiBuilder::new().max_rect(label_rect));
			ui.style_mut().visuals.override_text_color = Some(egui::Color32::WHITE);
			ui.add(label);
		}


		// Draw the slider
		let mut cur_time_secs = video.cur_time().as_secs_f64();
		let slider = egui::Slider::new(&mut cur_time_secs, 0.0..=duration.as_secs_f64())
			.smart_aim(false)
			.show_value(false);

		let slider_rect = {
			let slider_width = controls_rect.width();
			let slider_size = egui::vec2(slider_width, slider_height);
			egui::Rect::from_min_size(
				controls_rect.min + (controls_rect.size() - slider_size) / egui::vec2(2.0, 1.0),
				slider_size,
			)
		};
		let slider_bg = egui::Color32::from_rgba_unmultiplied(0, 0, 0, 100);
		let slider_trailing_bg = egui::Color32::from_rgba_unmultiplied(0, 0, 0, 50);
		let response = {
			let mut ui = ui.new_child(egui::UiBuilder::new().max_rect(slider_rect));
			let style = ui.style_mut();
			style.spacing.slider_width = slider_rect.width();
			style.spacing.slider_rail_height = slider_rect.height();
			style.spacing.interact_size.y = slider_rect.height();
			style.visuals.handle_shape = egui::style::HandleShape::Rect { aspect_ratio: 0.0 };
			style.visuals.slider_trailing_fill = true;
			style.visuals.selection.bg_fill = slider_trailing_bg;
			style.visuals.widgets.inactive.bg_fill = slider_bg;
			ui.add(slider)
		};

		// While dragging, start seeking
		match response.dragged() {
			true => video.set_seeking(),
			false => video.stop_seeking(),
		}

		// Then, if changed seek to it.
		if response.changed() {
			video.seek_to(Duration::from_secs_f64(cur_time_secs));
		}
	}

	fn preload_entry(&mut self, egui_ctx: &egui::Context, entry: &DirEntry) {
		self.with_entry(entry, |this, entry| try {
			let Some(data) = entry
				.try_data(&this.thread_pool, egui_ctx)
				.context("Unable to load entry data")?
			else {
				return Ok(());
			};

			match data {
				EntryData::Image(image) => image
					.load(&this.thread_pool, egui_ctx)
					.context("Unable to load image")?,
				EntryData::Video(video) =>
					if !video.started() {
						// TODO: Pausing the video here means we aren't actually
						//       preloading almost anything, just the thread initializing,
						//       we should make it so a single frame gets rendered here.
						video.pause();
						video.start();
					},
				EntryData::Other => this.remove_entry(entry),
			}

			this.loaded_entries.insert(entry.clone());
		});
	}

	/// Draws an entry
	fn draw_entry(&mut self, ui: &mut egui::Ui, input: DrawInput) -> DrawOutput {
		let mut output = DrawOutput {
			image_response: None,
			resize_size:    None,
		};

		let vertical_pan = self.vertical_pan_smooth * self.controls.keyboard_pan_smooth;
		self.vertical_pan_smooth -= vertical_pan;
		if self.vertical_pan_smooth.abs() < 1e-5 {
			self.vertical_pan_smooth = 0.0;
		}

		// Note: It's important we add the entry to the loaded *before*
		//       calling `try_data`, because otherwise we'd potentially
		//       miss it.
		self.loaded_entries.insert(input.entry.clone().into());
		let Some(data) = self.try_with_entry(input.entry, |this, entry| entry.try_data(&this.thread_pool, ui.ctx()))
		else {
			ui.centered_and_justified(|ui| {
				ui.weak("Loading...");
			});
			return output;
		};

		// Pre-load some entries
		// TODO: Not panic here
		let preload_before = self
			.dir_reader
			.before_entry(input.entry, self.preload_prev)
			.expect("Entry should be valid");
		let preload_after = self
			.dir_reader
			.after_entry(input.entry, self.preload_next)
			.expect("Entry should be valid");

		for entry in [preload_before, preload_after].into_iter().flatten() {
			self.preload_entry(ui.ctx(), &entry);
		}

		match data {
			EntryData::Video(video) => {
				if !video.started() {
					video.start();
				}

				let image_size = video.size();
				if image_size == egui::Vec2::ZERO {
					video.resume();
					ui.centered_and_justified(|ui| {
						ui.weak("Loading...");
					});
					return output;
				}

				// If it wasn't on-screen, start playing it and resize
				// TODO: This isn't always what we want, if the user paused it,
				//       then went to another image and came back, we probably
				//       don't want to be playing.
				if !video.set_onscreen() {
					video.resume();
					output.resize_size = Some(image_size);
				}

				if input.toggle_pause {
					match video.playing_status() {
						PlayingStatus::Playing => video.pause(),
						PlayingStatus::Paused => video.resume(),
						PlayingStatus::Seeking { .. } => (),
					}
				}

				let handle = video.handle();
				let draw_image = |ui: &mut egui::Ui, ui_rect: egui::Rect, uv_rect: egui::Rect| {
					egui::Image::new(&handle).uv(uv_rect).paint_at(ui, ui_rect);
				};

				output.image_response = Some(Self::draw_image(
					ui,
					draw_image,
					image_size,
					&mut self.pan_zoom,
					self.view_mode,
					input.window_response,
					vertical_pan,
					&self.controls,
				));
				self.draw_video_controls(ui, input, &video);
			},

			EntryData::Image(image) => {
				let Some(texture) = self.try_with_entry(input.entry, |this, _| {
					image.load(&this.thread_pool, ui.ctx())?;
					image.texture()
				}) else {
					ui.centered_and_justified(|ui| {
						ui.weak("Loading...");
					});
					return output;
				};

				let image_size = texture.size_vec2();
				let draw_image = |ui: &mut egui::Ui, ui_rect: egui::Rect, uv_rect: egui::Rect| {
					texture.paint_at(ui, uv_rect, ui_rect);
				};

				// Note: If we're in fullscreen, we shouldn't resize yet.
				//       If we did, the user might notice a flicker, because
				//       our resize size isn't the fullscreen size the user
				//       is using, and resizing won't change that.
				let is_fullscreen = ui.input(|input| input.viewport().fullscreen).unwrap_or(false);
				if !self.resized_image && !is_fullscreen {
					output.resize_size = Some(image_size);
					self.resized_image = true;
				}

				output.image_response = Some(Self::draw_image(
					ui,
					draw_image,
					image_size,
					&mut self.pan_zoom,
					self.view_mode,
					input.window_response,
					vertical_pan,
					&self.controls,
				));
			},

			EntryData::Other => self.remove_entry(input.entry),
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

			move_prev = input.consume_shortcut_key(self.shortcuts.prev);
			move_next = input.consume_shortcut_key(self.shortcuts.next);

			move_first = input.consume_shortcut_key(self.shortcuts.first);
			move_last = input.consume_shortcut_key(self.shortcuts.last);

			toggle_pause |= input.consume_shortcut_key(self.shortcuts.toggle_pause);
			toggle_pause |= input.pointer.button_clicked(egui::PointerButton::Primary);

			// Note: The order is important here, because by default `pan_up` and `fit_width_if_default`
			//       use the same key, so if we consumed it here before checking whether it's
			//       applicable, we'd not be able to pan up.
			let is_default_view_mode = self.view_mode == ViewMode::FitWindow &&
				self.pan_zoom.offset == egui::Vec2::ZERO &&
				self.pan_zoom.zoom == 0.0;
			if is_default_view_mode && input.consume_shortcut_key(self.shortcuts.fit_width_if_default) {
				self.view_mode = ViewMode::FitWidth;
			}

			if input.consume_shortcut_key(self.shortcuts.pan_up) {
				self.vertical_pan_smooth += 1.0;
			}
			if input.consume_shortcut_key(self.shortcuts.pan_down) {
				self.vertical_pan_smooth -= 1.0;
			}


			for (&view_mode, &key) in &self.shortcuts.view_modes {
				if input.consume_shortcut_key(key) {
					self.view_mode = view_mode;
					self.pan_zoom = PanZoom {
						offset: egui::Vec2::ZERO,
						zoom:   0.0,
					};
				}
			}
		});

		if !self.dir_reader.has_cur_entry() &&
			let Some(entry) = &self.prev_valid_entry
		{
			self.dir_reader.cur_entry_set(entry.clone());
		}

		let Some(mut cur_entry) = self.dir_reader.cur_entry() else {
			return;
		};

		if move_prev && let Some(entry) = self.dir_reader.cur_entry_set_prev() {
			let prev_entry = mem::replace(&mut cur_entry, entry);
			self.reset_on_change_entry(&prev_entry, &cur_entry);
		}
		if move_next && let Some(entry) = self.dir_reader.cur_entry_set_next() {
			let prev_entry = mem::replace(&mut cur_entry, entry);
			self.reset_on_change_entry(&prev_entry, &cur_entry);
		}
		if move_first && let Some(entry) = self.dir_reader.cur_entry_set_first() {
			let prev_entry = mem::replace(&mut cur_entry, entry);
			self.reset_on_change_entry(&prev_entry, &cur_entry);
		}
		if move_last && let Some(entry) = self.dir_reader.cur_entry_set_last() {
			let prev_entry = mem::replace(&mut cur_entry, entry);
			self.reset_on_change_entry(&prev_entry, &cur_entry);
		}

		let cur_entry = cur_entry;

		self.draw_info_window(ctx, Some(&cur_entry));

		let panel_output = egui::CentralPanel::default().frame(egui::Frame::NONE).show(ctx, |ui| {
			let window_response = ui.interact(
				egui::Rect::from_min_size(egui::Pos2::ZERO, ui.available_size()),
				egui::Id::new("whole-screen"),
				egui::Sense::all(),
			);


			let draw_input = DrawInput {
				entry: &cur_entry,
				window_response: &window_response,
				toggle_pause,
			};
			let draw_output = self.draw_entry(ui, draw_input);
			let response = match draw_output.image_response.clone() {
				Some(response) => response.union(window_response),
				None => window_response,
			};

			// TODO: Should we make this a native viewport?
			// TODO: These errors should be popups, not logs.
			egui::Popup::context_menu(&response).show(|ui| {
				if let EntrySource::Path(cur_entry_path) = cur_entry.source() {
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
				}

				ui.separator();

				if let EntrySource::Path(cur_entry_path) = cur_entry.source() {
					if ui.button("Copy path").clicked() {
						// TODO: Not copy a lossy string?
						ctx.send_cmd(egui::OutputCommand::CopyText(
							cur_entry_path.to_string_lossy().into_owned(),
						));
					}
				}

				if ui.button("Copy file name").clicked() {
					// TODO: Not copy a lossy string?
					ctx.send_cmd(egui::OutputCommand::CopyText(
						cur_entry
							.file_name()
							.expect("Entry had no file name")
							.to_string_lossy()
							.into_owned(),
					));
				}

				ui.separator();

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

				ui.separator();

				if ui.button("Settings").clicked() {
					self.settings_is_open = true;
				}
			});

			draw_output
		});
		let draw_output = panel_output.inner;

		// If it was drawn, set this as our previous valid entry
		if draw_output.image_response.is_some() {
			self.prev_valid_entry = Some(cur_entry.clone().into());
		}

		// Note: If we couldn't get the monitor size, ignore any resizes
		// TODO: This resize can make our window fall out the bottom or right of
		//       the monitor since we're resizing up to monitor size,
		//       but our position isn't guaranteed to be 0.
		//       Unfortunately, while we could set our position (outside of wayland),
		//       `egui` has no way to get the monitor position (only size), so
		//       we'd always move ourselves to the user's left/top-most monitor, which isn't
		//       correct.
		if let Some(image_size) = draw_output.resize_size &&
			let Some(monitor_size) = ctx.input(|input| input.viewport().monitor_size)
		{
			let scale = f32::min(monitor_size.x / image_size.x, monitor_size.y / image_size.y).min(1.0);
			let size = (image_size * scale).round_ui();

			tracing::info!("Resizing to {}x{}", size.x, size.y);
			ctx.send_viewport_cmd(egui::ViewportCommand::InnerSize(size));
		}

		// TODO: Don't change the title each frame?
		ctx.send_viewport_cmd(egui::ViewportCommand::Title(self.title(&cur_entry)));

		// TODO: Don't duplicate this by returning the fact that we want to fullscreen instead
		// TODO: Also allow double-clicking an empty part in the display list to fullscreen
		let is_fullscreen = ctx.input(|input| input.viewport().fullscreen).unwrap_or(false);
		if fullscreen {
			ctx.send_viewport_cmd(egui::ViewportCommand::Fullscreen(!is_fullscreen));
		}
	}

	fn draw_display_list(&mut self, ctx: &egui::Context) {
		let mut goto_entry = None;

		// If the current entry was playing, pause it
		// TODO: Not do this here
		if let Some(cur_entry) = self.dir_reader.cur_entry() &&
			let Some(EntryData::Video(video)) =
				self.try_with_entry(&cur_entry, |_, cur_entry| cur_entry.data_if_exists()) &&
			video.set_offscreen()
		{
			video.pause();
		}

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
									continue;
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
											enum Thumbnail {
												Image(EntryImageTexture),
												NonMedia,
											}
											let thumbnail = self.try_with_entry(&entry, |this, entry| try {
												let Some(thumbnail) = entry.thumbnail(
													&this.thread_pool,
													ui.ctx(),
													this.thumbnails_dir.path(),
												)?
												else {
													return Ok(None);
												};

												match thumbnail {
													EntryThumbnail::Image(image) =>
														image.texture()?.map(Thumbnail::Image),
													EntryThumbnail::NonMedia => Some(Thumbnail::NonMedia),
												}
											});

											match thumbnail {
												// TODO: Not have to manually paint the texture?
												Some(Thumbnail::Image(texture)) => {
													let texture_size = texture.size_vec2();
													let aspect_ratio = match texture_size.y > texture_size.x {
														true => egui::vec2(texture_size.x / texture_size.y, 1.0),
														false => egui::vec2(1.0, texture_size.y / texture_size.x),
													};

													let uv_rect =
														egui::Rect::from_min_size(egui::Pos2::ZERO, egui::Vec2::ONE);

													// TODO: Why is the next widget position offset?
													let ui_pos =
														ui.next_widget_position() + egui::vec2(0.0, image_size.y / 2.0);
													let ui_rect =
														egui::Rect::from_center_size(ui_pos, image_size * aspect_ratio);

													texture.paint_at(ui, uv_rect, ui_rect);
												},
												Some(Thumbnail::NonMedia) => self.remove_entry(&entry),
												None => (),
											}
											let response = ui.allocate_response(image_size, egui::Sense::click());

											if response.double_clicked() {
												goto_entry = Some(entry.clone());
											}

											let file_name_height_id = egui::Id::new(("display-list-hover", &entry));
											let file_name_height =
												ui.data(|data| data.get_temp(file_name_height_id)).unwrap_or(0.0);

											let spacing = ui.available_height() - file_name_height;
											if spacing > 0.0 {
												let response = ui.allocate_response(
													egui::vec2(ui.available_width(), spacing),
													egui::Sense::click(),
												);

												if response.double_clicked() {
													goto_entry = Some(entry.clone());
												}
											}

											if let Ok(file_name) = entry.file_name() {
												let label = egui::Label::new(file_name.display().to_string())
													.wrap_mode(egui::TextWrapMode::Truncate);

												let response = ui.add(label);
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
		// Receive and try to load any new entries
		// TODO: This should be configurable because it can do IO
		// Note: To be more efficient, we use `swap_remove` when removing entries
		//       here, which means we don't have a good way to iterate over all
		//       entries aside from manually by indexing.
		self.loading_entries.extend(self.new_entry_rx.try_iter());
		for loading_entry_idx in 0..self.loading_entries.len() {
			let Some(entry) = self.loading_entries.get(loading_entry_idx) else {
				break;
			};

			match entry.try_data(&self.thread_pool, ctx) {
				// If we successfully loaded it, remove it from loading,
				// and remove it from the list if it's non-media.
				Ok(Some(data)) => {
					let entry = self.loading_entries.swap_remove(loading_entry_idx);
					if matches!(data, EntryData::Other) {
						self.remove_entry(&entry);
					}
				},
				// If unloaded, go next
				Ok(None) => (),
				// If it errored, remove it from both loading and the list.
				Err(err) => {
					tracing::warn!("Unable to load entry {:?}, removing: {err:?}", entry.source().name());
					let entry = self.loading_entries.swap_remove(loading_entry_idx);
					self.remove_entry(&entry);
				},
			}
		}

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
			fullscreen = input.consume_shortcut_key(self.shortcuts.fullscreen);

			exit_fullscreen_or_quit = input.consume_shortcut_key(self.shortcuts.exit_fullscreen_or_quit);

			quit = input.consume_shortcut_key(self.shortcuts.quit);

			toggle_display_mode = input.consume_shortcut_key(self.shortcuts.toggle_display_mode);

			for (&kind, &key) in &self.shortcuts.sort {
				if input.consume_shortcut_key(key) {
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
		self.draw_config_window(ctx);

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
	entry_tx: mpsc::Sender<DirEntry>,
	ctx:      egui::Context,
}

impl dir_reader::Visitor for DirReaderVisitor {
	fn entry_added(&self, dir_entry: &DirEntry) {
		_ = self.entry_tx.send(dir_entry.clone());
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
	image_response: Option<egui::Response>,
	resize_size:    Option<egui::Vec2>,
}

#[derive(Clone, Copy, Debug)]
struct PanZoom {
	offset: egui::Vec2,
	zoom:   f32,
}

/// Settings tab
#[derive(PartialEq, Eq, Clone, Copy, Debug)]
#[derive(derive_more::Display)]
#[derive(strum::VariantArray)]
enum SettingsTab {
	#[display("General")]
	General,

	#[display("Shortcuts")]
	Shortcuts,
}

/// Shortcut key identifier
#[derive(PartialEq, Eq, Clone, Copy, Debug)]
#[derive(derive_more::Display)]
#[derive(strum::EnumDiscriminants)]
#[strum_discriminants(name(ShortcutKeyIdentInner))]
#[strum_discriminants(derive(strum::VariantArray))]
enum ShortcutKeyIdent {
	#[display("Previous image")]
	Prev,
	#[display("Next image")]
	Next,
	#[display("Pan up")]
	PanUp,
	#[display("Pan down")]
	PanDown,
	#[display("Fit to width (if default)")]
	FitWidthIfDefault,
	#[display("First image")]
	First,
	#[display("Last image")]
	Last,
	#[display("Fullscreen")]
	Fullscreen,
	#[display("Exit fullscreen or quit")]
	ExitFullscreenOrQuit,
	#[display("Toggle pause")]
	TogglePause,
	#[display("Quit")]
	Quit,
	#[display("Toggle display mode")]
	ToggleDisplayMode,
	#[display("Sort by {}", Self::sort_name(*_0))]
	Sort(SortOrderKind),
	#[display("Fit to {}", Self::view_name(*_0))]
	ViewModes(ViewMode),
}

impl ShortcutKeyIdent {
	gen fn variants() -> Self {
		for inner in ShortcutKeyIdentInner::VARIANTS {
			match inner {
				ShortcutKeyIdentInner::Prev => yield Self::Prev,
				ShortcutKeyIdentInner::Next => yield Self::Next,
				ShortcutKeyIdentInner::PanUp => yield Self::PanUp,
				ShortcutKeyIdentInner::PanDown => yield Self::PanDown,
				ShortcutKeyIdentInner::FitWidthIfDefault => yield Self::FitWidthIfDefault,
				ShortcutKeyIdentInner::First => yield Self::First,
				ShortcutKeyIdentInner::Last => yield Self::Last,
				ShortcutKeyIdentInner::Fullscreen => yield Self::Fullscreen,
				ShortcutKeyIdentInner::ExitFullscreenOrQuit => yield Self::ExitFullscreenOrQuit,
				ShortcutKeyIdentInner::TogglePause => yield Self::TogglePause,
				ShortcutKeyIdentInner::Quit => yield Self::Quit,
				ShortcutKeyIdentInner::ToggleDisplayMode => yield Self::ToggleDisplayMode,
				ShortcutKeyIdentInner::Sort =>
					for &kind in SortOrderKind::VARIANTS {
						yield Self::Sort(kind);
					},
				ShortcutKeyIdentInner::ViewModes =>
					for &view_mode in ViewMode::VARIANTS {
						yield Self::ViewModes(view_mode);
					},
			}
		}
	}

	const fn sort_name(kind: SortOrderKind) -> &'static str {
		match kind {
			SortOrderKind::FileName => "file name",
			SortOrderKind::ModificationDate => "modification date",
			SortOrderKind::Size => "size",
		}
	}

	const fn view_name(mode: ViewMode) -> &'static str {
		match mode {
			ViewMode::FitWindow => "window",
			ViewMode::FitWidth => "width",
			ViewMode::ActualSize => "actual size",
		}
	}

	pub fn get(self, shortcuts: &mut Shortcuts) -> &mut ShortcutKey {
		match self {
			Self::Prev => &mut shortcuts.prev,
			Self::Next => &mut shortcuts.next,
			Self::PanUp => &mut shortcuts.pan_up,
			Self::PanDown => &mut shortcuts.pan_down,
			Self::FitWidthIfDefault => &mut shortcuts.fit_width_if_default,
			Self::First => &mut shortcuts.first,
			Self::Last => &mut shortcuts.last,
			Self::Fullscreen => &mut shortcuts.fullscreen,
			Self::ExitFullscreenOrQuit => &mut shortcuts.exit_fullscreen_or_quit,
			Self::TogglePause => &mut shortcuts.toggle_pause,
			Self::Quit => &mut shortcuts.quit,
			Self::ToggleDisplayMode => &mut shortcuts.toggle_display_mode,
			Self::Sort(kind) => shortcuts.sort.entry(kind).or_insert(ShortcutKey::UNBOUND),
			Self::ViewModes(view_mode) => shortcuts.view_modes.entry(view_mode).or_insert(ShortcutKey::UNBOUND),
		}
	}
}

#[derive(Clone, Debug)]
enum ThumbnailsDir {
	Auto(Arc<Path>),
	Specified(Arc<Path>),
}

impl ThumbnailsDir {
	const fn path(&self) -> &Arc<Path> {
		match self {
			Self::Auto(path) | Self::Specified(path) => path,
		}
	}
}
