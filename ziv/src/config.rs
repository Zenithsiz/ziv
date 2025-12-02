//! Configuration

// Imports
use {crate::shortcut::Shortcuts, std::path::PathBuf};

/// Configuration
#[derive(Clone, Debug)]
#[derive(serde::Serialize, serde::Deserialize)]
pub struct Config {
	/// Thumbnails cache
	#[serde(skip_serializing_if = "Option::is_none", default)]
	pub thumbnails_cache: Option<PathBuf>,

	/// Extensions
	pub exts: Exts,

	/// Shortcuts
	pub shortcuts: Shortcuts,
}

/// Extensions
// Note: Fields are required to avoid an empty config file meaning
//       that no extensions are allowed, as opposed to default.
#[derive(Clone, Debug)]
#[derive(serde::Serialize, serde::Deserialize)]
pub struct Exts {
	/// Image
	pub image: Vec<String>,

	/// Video
	pub video: Vec<String>,
}

impl Default for Config {
	fn default() -> Self {
		Self {
			thumbnails_cache: None,
			exts:             Exts {
				image: DEFAULT_IMAGE_EXTS.iter().copied().map(String::from).collect(),
				video: DEFAULT_VIDEO_EXTS.iter().copied().map(String::from).collect(),
			},
			shortcuts:        Shortcuts::default(),
		}
	}
}

/// Default image extensions
pub const DEFAULT_IMAGE_EXTS: &[&str] = &["jpg", "jpeg", "png", "webp"];

/// Default video extensions
pub const DEFAULT_VIDEO_EXTS: &[&str] = &["gif", "mkv", "mp4", "webm"];
