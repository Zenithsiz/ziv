//! Directory entry

// Imports
use {
	super::{SortOrder, SortOrderKind},
	crate::util::AppError,
	app_error::Context,
	core::cmp::Ordering,
	std::{
		fs::{self, Metadata},
		path::{Path, PathBuf},
		sync::{Arc, OnceLock},
		time::SystemTime,
	},
};

#[derive(Debug)]
struct Inner {
	path:          PathBuf,
	// TODO: Support non-utf-8 file names
	file_name:     OnceLock<String>,
	metadata:      OnceLock<Metadata>,
	modified_date: OnceLock<SystemTime>,
}

#[derive(Clone, Debug)]
pub struct DirEntry(Arc<Inner>);

impl DirEntry {
	/// Creates a new directory entry
	pub fn new(path: PathBuf) -> Self {
		Self(Arc::new(Inner {
			path,
			file_name: OnceLock::new(),
			metadata: OnceLock::new(),
			modified_date: OnceLock::new(),
		}))
	}

	/// Returns this entry's path
	pub fn path(&self) -> &Path {
		&self.0.path
	}

	pub(super) fn update_file_name(&self) -> Result<&String, AppError> {
		self.0.file_name.get_or_try_init(|| try {
			self.path()
				.file_name()
				.context("Missing file name")?
				.to_str()
				.context("File name was non-utf8")?
				.to_owned()
		})
	}

	pub(super) fn update_metadata(&self) -> Result<&Metadata, AppError> {
		self.0
			.metadata
			.get_or_try_init(|| fs::metadata(self.path()).context("Unable to get metadata"))
	}

	pub(super) fn update_modified_date(&self) -> Result<&SystemTime, AppError> {
		let metadata = self.update_metadata().context("Unable to get metadata")?;
		self.0
			.modified_date
			.get_or_try_init(|| metadata.modified().context("Unable to get system time"))
	}

	/// Compares two directory entries according to a sort order
	pub fn cmp_with(&self, other: &Self, order: SortOrder) -> Option<Ordering> {
		let cmp = match order.kind {
			SortOrderKind::FileName => natord::compare(self.0.file_name.get()?, other.0.file_name.get()?),
			SortOrderKind::ModificationDate =>
				SystemTime::cmp(self.0.modified_date.get()?, other.0.modified_date.get()?),
		};

		match order.reverse {
			true => Some(cmp.reverse()),
			false => Some(cmp),
		}
	}

	/// Loads the necessary fields for `order`
	pub(super) fn load_for_order(&self, order: SortOrder) -> Result<(), AppError> {
		match order.kind {
			SortOrderKind::FileName => _ = self.update_file_name().context("Unable to get file name")?,
			SortOrderKind::ModificationDate =>
				_ = self.update_modified_date().context("Unable to get modified date")?,
		}

		Ok(())
	}
}

impl PartialEq for DirEntry {
	fn eq(&self, other: &Self) -> bool {
		Arc::ptr_eq(&self.0, &other.0)
	}
}

impl Eq for DirEntry {}
