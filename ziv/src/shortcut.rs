//! Shortcut keys

// Imports
use {
	crate::{ViewMode, dir_reader::SortOrderKind, util::hashmap_serialize_sorted},
	std::collections::HashMap,
	strum::VariantArray,
};

/// Shortcut keys
// TODO: Allow modifiers here.
#[derive(Clone, Debug)]
#[derive(serde::Serialize, serde::Deserialize)]
pub struct Shortcuts {
	pub prev: egui::Key,
	pub next: egui::Key,

	pub pan_up:   egui::Key,
	pub pan_down: egui::Key,

	pub fit_width_if_default: egui::Key,

	pub first: egui::Key,
	pub last:  egui::Key,

	pub fullscreen:              egui::Key,
	pub exit_fullscreen_or_quit: egui::Key,

	pub toggle_pause: egui::Key,
	pub quit:         egui::Key,

	pub toggle_display_mode: egui::Key,

	#[serde(serialize_with = "hashmap_serialize_sorted")]
	pub sort: HashMap<SortOrderKind, egui::Key>,

	#[serde(serialize_with = "hashmap_serialize_sorted")]
	pub view_modes: HashMap<ViewMode, egui::Key>,
}

impl Default for Shortcuts {
	fn default() -> Self {
		Self {
			prev:                    egui::Key::ArrowLeft,
			next:                    egui::Key::ArrowRight,
			pan_up:                  egui::Key::ArrowUp,
			pan_down:                egui::Key::ArrowDown,
			fit_width_if_default:    egui::Key::ArrowUp,
			first:                   egui::Key::Home,
			last:                    egui::Key::End,
			fullscreen:              egui::Key::F,
			exit_fullscreen_or_quit: egui::Key::Escape,
			toggle_pause:            egui::Key::Space,
			quit:                    egui::Key::Q,
			toggle_display_mode:     egui::Key::Backspace,
			sort:                    SortOrderKind::VARIANTS
				.iter()
				.map(|&kind| {
					let key = match kind {
						SortOrderKind::FileName => egui::Key::F1,
						SortOrderKind::ModificationDate => egui::Key::F2,
						SortOrderKind::Size => egui::Key::F3,
					};

					(kind, key)
				})
				.collect(),
			view_modes:              ViewMode::VARIANTS
				.iter()
				.map(|&kind| {
					let key = match kind {
						ViewMode::FitWindow => egui::Key::Num1,
						ViewMode::FitWidth => egui::Key::Num2,
						ViewMode::ActualSize => egui::Key::Num3,
					};

					(kind, key)
				})
				.collect(),
		}
	}
}
