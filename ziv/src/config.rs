//! Configuration

/// Configuration
#[derive(Clone, Debug)]
#[derive(serde::Serialize, serde::Deserialize)]
pub struct Config {}

#[expect(clippy::derivable_impls, reason = "We want to be explicit with the default config")]
impl Default for Config {
	fn default() -> Self {
		Self {}
	}
}
