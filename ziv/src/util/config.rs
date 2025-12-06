//! Config read/write

// Imports
use {
	crate::AppError,
	app_error::Context,
	serde::{Serialize, de::DeserializeOwned},
	std::{fs, path::Path},
};

/// Tries to load the config
///
/// If unable to, attempts to create a default config.
pub fn get_or_create_with<T>(path: &Path, default: impl FnOnce() -> T) -> T
where
	T: Serialize + DeserializeOwned,
{
	match self::load(path) {
		Ok(config) => config,
		Err(err) => {
			tracing::warn!("Unable to load config from {path:?}, using default: {}", err.pretty());
			let config = default();

			// If the config file doesn't exist, write the default
			// Note: If we're unable to check for existence, we assume it does exist, so we don't override anything
			if !fs::exists(path).unwrap_or(true) &&
				let Err(err) = self::save(&config, path)
			{
				tracing::warn!("Unable to write default config to {path:?}: {}", err.pretty());
			}

			config
		},
	}
}

/// Loads the config
fn load<T>(path: &Path) -> Result<T, AppError>
where
	T: DeserializeOwned,
{
	tracing::debug!("Loading config from path: {path:?}");

	let config_toml = fs::read_to_string(path).context("Unable to open file")?;
	let config = toml::from_str(&config_toml).context("Unable to parse config")?;
	Ok(config)
}

/// Saves the configuration to path
pub fn save<T>(config: &T, path: &Path) -> Result<(), AppError>
where
	T: Serialize,
{
	tracing::debug!("Saving config in path: {path:?}");

	super::create_parent(path).context("Unable to create config parent directory")?;
	let config_toml = toml::to_string(config).context("Unable to serialize config")?;
	fs::write(path, config_toml.as_bytes()).context("Unable to write config")?;

	Ok(())
}
