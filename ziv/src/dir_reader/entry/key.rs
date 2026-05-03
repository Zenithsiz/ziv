//! Directory entry sort key

// Imports
use {
	super::DirEntry,
	crate::util::{AppError, PartialEqOrd},
	app_error::Context,
	std::{path::PathBuf, random, time::SystemTime},
};

/// Sort key for a directory entry.
pub trait Key: Sized {
	/// Creates a new key from an entry
	fn from_entry(entry: &DirEntry) -> Result<Self, AppError>;
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
	fn from_entry(entry: &DirEntry) -> Result<Self, AppError> {
		let file_name = entry.file_name().context("Unable to get file name")?;

		Ok(Self(file_name))
	}
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct ModificationDate(SystemTime);

impl Key for ModificationDate {
	fn from_entry(entry: &DirEntry) -> Result<Self, AppError> {
		let modified_date = entry.modified_date_if_loaded()?.context("Missing modified date")?;

		Ok(Self(modified_date))
	}
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct Size(u64);

impl Key for Size {
	fn from_entry(entry: &DirEntry) -> Result<Self, AppError> {
		let size = entry.size_if_loaded()?.context("Missing size")?;

		Ok(Self(size))
	}
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct ResolutionWidth(usize);

impl Key for ResolutionWidth {
	fn from_entry(entry: &DirEntry) -> Result<Self, AppError> {
		let resolution = entry.resolution_if_loaded()?.context("Missing resolution")?;

		Ok(Self(resolution.width))
	}
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct ResolutionHeight(usize);

impl Key for ResolutionHeight {
	fn from_entry(entry: &DirEntry) -> Result<Self, AppError> {
		let resolution = entry.resolution_if_loaded()?.context("Missing resolution")?;

		Ok(Self(resolution.height))
	}
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct Random(u64);

impl Key for Random {
	fn from_entry(_entry: &DirEntry) -> Result<Self, AppError> {
		let random = random::random(..);

		Ok(Self(random))
	}
}
