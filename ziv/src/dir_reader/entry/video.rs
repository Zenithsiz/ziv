//! Entry video

// Imports
use {
	crate::util::AppError,
	app_error::Context,
	parking_lot::{Mutex, MutexGuard},
	std::{fmt, path::Path, sync::Arc},
};

struct Inner {
	player:   egui_video::Player,
	onscreen: bool,
}

impl fmt::Debug for Inner {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		f.debug_struct("EntryVideo")
			.field("handle", &self.player.texture_handle.id())
			.field("on_screen", &self.onscreen)
			.finish()
	}
}

/// Entry video
#[derive(Clone, Debug)]
pub struct EntryVideo {
	inner: Arc<Mutex<Inner>>,
}

impl EntryVideo {
	/// Creates a new video, not playing
	pub fn new(egui_ctx: &egui::Context, path: &Path) -> Result<Self, AppError> {
		let player = egui_video::Player::new(egui_ctx, path.to_path_buf())
			.map_err(|err| AppError::from(&*err.into_boxed_dyn_error()))
			.context("Unable to create player")?;

		Ok(Self {
			inner: Arc::new(Mutex::new(Inner {
				player,
				onscreen: false,
			})),
		})
	}

	/// Locks this video
	pub fn lock(&self) -> EntryVideoGuard<'_> {
		EntryVideoGuard(self.inner.lock())
	}
}

#[derive(Debug)]
pub struct EntryVideoGuard<'a>(MutexGuard<'a, Inner>);

impl EntryVideoGuard<'_> {
	/// Returns the player
	pub fn player(&mut self) -> &mut egui_video::Player {
		&mut self.0.player
	}

	/// Sets this video as on-screen.
	///
	/// Returns whether we were already on-screen
	pub fn set_onscreen(&mut self) -> bool {
		match self.0.onscreen {
			true => true,
			false => {
				self.0.onscreen = true;
				false
			},
		}
	}

	/// Sets this video as off-screen
	///
	/// Returns whether we were on-screen
	pub fn set_offscreen(&mut self) -> bool {
		let was_onscreen = self.0.onscreen;
		self.0.onscreen = false;

		was_onscreen
	}
}
