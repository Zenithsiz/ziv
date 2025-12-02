//! Project directories

// Imports
use {
	crate::util::AppError,
	app_error::Context,
	directories::ProjectDirs,
	std::{
		path::{Path, PathBuf},
		sync::OnceLock,
	},
};

/// Directories
#[derive(Debug)]
pub struct Dirs {
	/// Project dirs
	inner: ProjectDirs,

	/// Thumbnail directory
	// TODO: Use the xdg spec thumbnail directory instead of our own?
	thumbnails: OnceLock<PathBuf>,
}

impl Dirs {
	/// Creates the directories
	pub fn new() -> Result<Self, AppError> {
		let inner = ProjectDirs::from("", "", "ziv").context("Unable to determine home directory")?;

		Ok(Self {
			inner,
			thumbnails: OnceLock::new(),
		})
	}

	/// Returns the thumbnails directory
	pub fn thumbnails(&self) -> &Path {
		self.thumbnails.get_or_init(|| {
			let path = self.inner.cache_dir().join("thumbnails");
			tracing::info!("Thumbnails directory: {path:?}");
			path
		})
	}
}
