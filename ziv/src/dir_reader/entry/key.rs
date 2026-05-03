//! Directory entry sort key

// Imports
use {
	super::{DirEntry, EntryData, EntryDisplay, EntryLoadedDisplays},
	crate::util::{AppError, PartialEqOrd},
	app_error::Context,
	std::{marker::ConstParamTy, path::PathBuf, random, time::SystemTime},
};

/// Sort key for a directory entry.
pub trait Key: Sized {
	/// Creates a new key from an entry
	fn from_entry(entry: &DirEntry, loaded_displays: &EntryLoadedDisplays) -> Result<Self, AppError>;

	/// Returns if an entry is loaded for this key
	fn is_loaded(entry: &DirEntry, loaded_displays: &EntryLoadedDisplays) -> Result<bool, AppError>;

	/// Loads this entry for this key
	fn load(entry: &DirEntry, loaded_displays: &EntryLoadedDisplays) -> Result<(), AppError>;
}

// TODO: Once we actually show directories, we need to ensure
//       all of these sort directories before other files.

#[derive(PartialEqOrd, Debug)]
pub struct FileName(PathBuf);

impl Ord for FileName {
	fn cmp(&self, other: &Self) -> core::cmp::Ordering {
		natord::compare_iter(
			self.0.as_os_str().as_encoded_bytes().iter(),
			other.0.as_os_str().as_encoded_bytes().iter(),
			|&c| c.is_ascii_whitespace(),
			|&l, &r| l.cmp(r),
			|&c| c.is_ascii_digit().then(|| isize::from(c - b'0')),
		)
	}
}

impl Key for FileName {
	fn from_entry(entry: &DirEntry, _loaded_displays: &EntryLoadedDisplays) -> Result<Self, AppError> {
		let file_name = entry.file_name().context("Unable to get file name")?;

		Ok(Self(file_name))
	}

	fn is_loaded(_entry: &DirEntry, _loaded_displays: &EntryLoadedDisplays) -> Result<bool, AppError> {
		Ok(true)
	}

	fn load(_entry: &DirEntry, _loaded_displays: &EntryLoadedDisplays) -> Result<(), AppError> {
		Ok(())
	}
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct ModificationDate(SystemTime);

impl Key for ModificationDate {
	fn from_entry(entry: &DirEntry, _loaded_displays: &EntryLoadedDisplays) -> Result<Self, AppError> {
		let modified_date = entry.modified_date_if_loaded()?.context("Missing modified date")?;

		Ok(Self(modified_date))
	}

	fn is_loaded(entry: &DirEntry, _loaded_displays: &EntryLoadedDisplays) -> Result<bool, AppError> {
		Ok(entry.0.metadata.is_loaded())
	}

	fn load(entry: &DirEntry, _loaded_displays: &EntryLoadedDisplays) -> Result<(), AppError> {
		entry.modified_date_blocking()?;
		Ok(())
	}
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct Size(u64);

impl Key for Size {
	fn from_entry(entry: &DirEntry, _loaded_displays: &EntryLoadedDisplays) -> Result<Self, AppError> {
		let size = entry.size_if_loaded()?.context("Missing size")?;

		Ok(Self(size))
	}

	fn is_loaded(entry: &DirEntry, _loaded_displays: &EntryLoadedDisplays) -> Result<bool, AppError> {
		Ok(entry.0.metadata.is_loaded())
	}

	fn load(entry: &DirEntry, _loaded_displays: &EntryLoadedDisplays) -> Result<(), AppError> {
		entry.size_blocking()?;
		Ok(())
	}
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
#[derive(ConstParamTy)]
pub enum ResolutionDir {
	Width,
	Height,
}

pub type ResolutionWidth = Resolution<{ ResolutionDir::Width }>;
pub type ResolutionHeight = Resolution<{ ResolutionDir::Height }>;

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct Resolution<const DIR: ResolutionDir>(usize);

impl<const DIR: ResolutionDir> Key for Resolution<DIR> {
	fn from_entry(entry: &DirEntry, loaded_displays: &EntryLoadedDisplays) -> Result<Self, AppError> {
		let resolution = entry
			.resolution_if_loaded(loaded_displays)?
			.context("Missing resolution")?;

		let size = match DIR {
			ResolutionDir::Width => resolution.width,
			ResolutionDir::Height => resolution.height,
		};

		Ok(Self(size))
	}

	fn is_loaded(entry: &DirEntry, loaded_displays: &EntryLoadedDisplays) -> Result<bool, AppError> {
		let Some(data) = entry.0.data.try_get()? else {
			return Ok(false);
		};

		let is_loaded = match data {
			EntryData::DisplayGuess(_) | EntryData::Unknown => {
				let Some(display) = loaded_displays.get_if_loaded(entry)? else {
					return Ok(false);
				};

				match display {
					EntryDisplay::Image(image) => image.resolution_is_loaded(),
					EntryDisplay::Video(video) => video.resolution_is_loaded(),
				}
			},
			EntryData::NonMedia => true,
		};

		Ok(is_loaded)
	}

	fn load(entry: &DirEntry, loaded_displays: &EntryLoadedDisplays) -> Result<(), AppError> {
		let data = entry.data_blocking()?;
		match data {
			EntryData::DisplayGuess(_) | EntryData::Unknown => match loaded_displays.get_blocking(entry)? {
				EntryDisplay::Image(image) => _ = image.resolution_blocking()?,
				EntryDisplay::Video(video) => _ = video.resolution_blocking()?,
			},
			EntryData::NonMedia => (),
		}

		Ok(())
	}
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct Random(u64);

impl Key for Random {
	fn from_entry(_entry: &DirEntry, _loaded_displays: &EntryLoadedDisplays) -> Result<Self, AppError> {
		let random = random::random(..);

		Ok(Self(random))
	}

	fn is_loaded(_entry: &DirEntry, _loaded_displays: &EntryLoadedDisplays) -> Result<bool, AppError> {
		Ok(true)
	}

	fn load(_entry: &DirEntry, _loaded_displays: &EntryLoadedDisplays) -> Result<(), AppError> {
		Ok(())
	}
}
