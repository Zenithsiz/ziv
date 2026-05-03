//! Directory entry sort key

// Imports
use {
	super::DirEntry,
	crate::util::{AppError, PartialEqOrd},
	app_error::Context,
	std::{path::PathBuf, random, time::SystemTime},
};

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

impl FileName {
	pub fn new(entry: &DirEntry) -> Result<Self, AppError> {
		let file_name = entry.file_name().context("Unable to get file name")?;

		Ok(Self(file_name))
	}
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct ModificationDate(SystemTime);

impl ModificationDate {
	pub fn new(entry: &DirEntry) -> Result<Self, AppError> {
		let modified_date = entry.modified_date_if_loaded()?.context("Missing modified date")?;

		Ok(Self(modified_date))
	}
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct Size(u64);

impl Size {
	pub fn new(entry: &DirEntry) -> Result<Self, AppError> {
		let size = entry.size_if_loaded()?.context("Missing size")?;

		Ok(Self(size))
	}
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct ResolutionWidth(usize);

impl ResolutionWidth {
	pub fn new(entry: &DirEntry) -> Result<Self, AppError> {
		let resolution = entry.resolution_if_loaded()?.context("Missing resolution")?;

		Ok(Self(resolution.width))
	}
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct ResolutionHeight(usize);

impl ResolutionHeight {
	pub fn new(entry: &DirEntry) -> Result<Self, AppError> {
		let resolution = entry.resolution_if_loaded()?.context("Missing resolution")?;

		Ok(Self(resolution.height))
	}
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct Random(u64);

impl Random {
	#[expect(
		clippy::unnecessary_wraps,
		reason = "We want all sort orders to have the same signature"
	)]
	pub fn new(_entry: &DirEntry) -> Result<Self, AppError> {
		let random = random::random(..);

		Ok(Self(random))
	}
}
