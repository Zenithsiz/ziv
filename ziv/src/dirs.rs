//! Project directories

// Imports
use std::path::PathBuf;

/// Directories
#[derive(Debug)]
pub struct Dirs {
	/// Cache directory
	_cache_dir: PathBuf,
}

impl Dirs {
	/// Creates new directories from a few root paths
	pub const fn new(cache_dir: PathBuf) -> Self {
		Self { _cache_dir: cache_dir }
	}
}
