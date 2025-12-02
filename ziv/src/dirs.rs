//! Project directories

// Imports
use {
	crate::util::AppError,
	app_error::Context,
	directories::ProjectDirs,
	std::{
		path::Path,
		sync::{Arc, OnceLock},
	},
};

/// Directories
#[derive(Debug)]
pub struct Dirs {
	/// Project dirs
	inner: ProjectDirs,

	/// Thumbnail directory
	// TODO: Use the xdg spec thumbnail directory instead of our own?
	thumbnails: OnceLock<Arc<Path>>,

	/// Config file
	// TODO: Use the xdg spec thumbnail directory instead of our own?
	config: OnceLock<Arc<Path>>,
}

impl Dirs {
	/// Creates the directories
	pub fn new() -> Result<Self, AppError> {
		let inner = ProjectDirs::from("", "", "ziv").context("Unable to determine home directory")?;

		Ok(Self {
			inner,
			thumbnails: OnceLock::new(),
			config: OnceLock::new(),
		})
	}

	/// Returns the thumbnails directory
	pub fn thumbnails(&self) -> &Arc<Path> {
		self.thumbnails.get_or_init(|| {
			let path = self.inner.cache_dir().join("thumbnails");
			tracing::info!("Thumbnails directory: {path:?}");
			path.into()
		})
	}

	/// Returns the config file
	pub fn config(&self) -> &Arc<Path> {
		self.config.get_or_init(|| {
			let path = self.inner.config_dir().join("config.toml");
			tracing::info!("Configuration file: {path:?}");
			path.into()
		})
	}
}
