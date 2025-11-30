//! Project directories

// Imports
use std::{
	path::{Path, PathBuf},
	sync::OnceLock,
};

/// Directories
#[derive(Debug)]
pub struct Dirs {
	/// Cache directory
	cache_dir: PathBuf,

	/// Thumbnail directory
	// TODO: Use the xdg spec thumbnail directory instead of our own?
	thumbnails: OnceLock<PathBuf>,
}

impl Dirs {
	/// Creates new directories from a few root paths
	pub const fn new(cache_dir: PathBuf) -> Self {
		Self {
			cache_dir,
			thumbnails: OnceLock::new(),
		}
	}

	/// Returns the thumbnails directory
	pub fn thumbnails(&self) -> &Path {
		self.thumbnails.get_or_init(|| {
			let path = self.cache_dir.join("thumbnails");
			tracing::info!("Thumbnails directory: {path:?}");
			path
		})
	}
}
