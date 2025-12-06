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

	/// Preload previous/next
	pub preload: [usize; 2],

	/// Extensions
	pub exts: Exts,

	/// Shortcuts
	pub shortcuts: Shortcuts,

	/// Controls
	pub controls: Controls,
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

/// Controls
#[derive(Clone, Debug)]
#[derive(serde::Serialize, serde::Deserialize)]
pub struct Controls {
	/// Zoom sensitivity
	pub zoom_sensitivity: f32,

	/// Scroll sensitivity
	pub scroll_sensitivity: f32,

	/// Keyboard pan sensitivity
	pub keyboard_pan_sensitivity: f32,

	/// Keyboard pan smooth
	pub keyboard_pan_smooth: f32,
}

impl Default for Config {
	fn default() -> Self {
		Self {
			thumbnails_cache: None,
			preload:          [1, 1],
			exts:             Exts {
				image: DEFAULT_IMAGE_EXTS.iter().copied().map(String::from).collect(),
				video: DEFAULT_VIDEO_EXTS.iter().copied().map(String::from).collect(),
			},
			shortcuts:        Shortcuts::default(),
			controls:         Controls {
				zoom_sensitivity:         200.0,
				scroll_sensitivity:       2.0,
				keyboard_pan_sensitivity: 0.2,
				keyboard_pan_smooth:      0.25,
			},
		}
	}
}

/// Default image extensions
pub const DEFAULT_IMAGE_EXTS: &[&str] = &["jpg", "jpeg", "png", "webp"];

/// Default video extensions
pub const DEFAULT_VIDEO_EXTS: &[&str] = &["gif", "mkv", "mp4", "webm"];
