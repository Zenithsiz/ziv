//! Configuration

// Imports
use std::path::PathBuf;

/// Configuration
#[derive(Clone, Debug)]
#[derive(serde::Serialize, serde::Deserialize)]
pub struct Config {
	/// Thumbnails cache
	#[serde(skip_serializing_if = "Option::is_none", default)]
	pub thumbnails_cache: Option<PathBuf>,
}

#[expect(clippy::derivable_impls, reason = "We want to be explicit with the default config")]
impl Default for Config {
	fn default() -> Self {
		Self { thumbnails_cache: None }
	}
}
