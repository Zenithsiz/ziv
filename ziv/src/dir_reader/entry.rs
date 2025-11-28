//! Directory entry

// Imports
use {
	super::{SortOrder, SortOrderKind},
	crate::util::AppError,
	app_error::Context,
	core::cmp::Ordering,
	parking_lot::Mutex,
	std::{
		fs::{self, Metadata},
		path::{Path, PathBuf},
		sync::{Arc, OnceLock},
		time::SystemTime,
	},
};

#[derive(Debug)]
struct Inner {
	path: Mutex<Arc<Path>>,

	// TODO: Support non-utf-8 file names
	file_name:     OnceLock<String>,
	metadata:      OnceLock<Metadata>,
	modified_date: OnceLock<SystemTime>,
	size:          OnceLock<u64>,
	image_details: Mutex<Option<ImageDetails>>,
}

impl Inner {
	fn path(&self) -> Arc<Path> {
		Arc::clone(&self.path.lock())
	}

	fn update_file_name(&self) -> Result<&String, AppError> {
		self.file_name.get_or_try_init(|| try {
			let path = self.path();
			path.file_name()
				.context("Missing file name")?
				.to_str()
				.context("File name was non-utf8")?
				.to_owned()
		})
	}

	fn update_metadata(&self) -> Result<&Metadata, AppError> {
		self.metadata.get_or_try_init(|| {
			let path = self.path();
			fs::metadata(path).context("Unable to get metadata")
		})
	}

	fn update_modified_date(&self) -> Result<&SystemTime, AppError> {
		self.modified_date.get_or_try_init(|| {
			self.update_metadata()
				.context("Unable to get metadata")?
				.modified()
				.context("Unable to get system time")
		})
	}

	fn update_size(&self) -> Result<&u64, AppError> {
		self.size.get_or_try_init(|| {
			let metadata = self.update_metadata().context("Unable to get metadata")?;
			Ok(metadata.len())
		})
	}
}

#[derive(Clone, derive_more::Debug)]
pub struct DirEntry(Arc<Inner>);

impl DirEntry {
	/// Creates a new directory entry
	pub(super) fn new(path: impl Into<Arc<Path>>) -> Self {
		Self(Arc::new(Inner {
			path:          Mutex::new(path.into()),
			file_name:     OnceLock::new(),
			metadata:      OnceLock::new(),
			modified_date: OnceLock::new(),
			size:          OnceLock::new(),
			image_details: Mutex::new(None),
		}))
	}

	/// Returns this entry's path
	pub fn path(&self) -> Arc<Path> {
		self.0.path()
	}

	/// Renames this entry
	pub fn rename(&self, path: PathBuf) {
		*self.0.path.lock() = path.into();
	}

	/// Sets the metadata of this entry
	pub fn set_metadata(&self, metadata: fs::Metadata) {
		_ = self.0.metadata.set(metadata);
	}

	/// Returns the image details of this entry
	pub fn image_details(&self) -> Option<ImageDetails> {
		self.0.image_details.lock().clone()
	}

	/// Returns if this entry contains image details
	pub fn has_image_details(&self) -> bool {
		self.0.image_details.lock().is_some()
	}

	/// Sets the image details of this entry
	pub fn set_image_details(&self, image_details: ImageDetails) {
		*self.0.image_details.lock() = Some(image_details);
	}

	/// Returns this image's file size
	pub fn size(&self) -> Result<u64, AppError> {
		// TODO: This can block, we should just try to get the field
		//       and have the user load it elsewhere.
		self.0.update_size().copied()
	}

	/// Compares two directory entries according to a sort order
	pub fn cmp_with(&self, other: &Self, order: SortOrder) -> Option<Ordering> {
		let cmp = match order.kind {
			SortOrderKind::FileName => natord::compare(self.0.file_name.get()?, other.0.file_name.get()?),
			SortOrderKind::ModificationDate =>
				SystemTime::cmp(self.0.modified_date.get()?, other.0.modified_date.get()?),
			SortOrderKind::Size => u64::cmp(self.0.size.get()?, other.0.size.get()?),
		};

		match order.reverse {
			true => Some(cmp.reverse()),
			false => Some(cmp),
		}
	}

	/// Loads the necessary fields for `order`
	pub(super) fn load_for_order(&self, order: SortOrder) -> Result<(), AppError> {
		match order.kind {
			SortOrderKind::FileName => _ = self.0.update_file_name().context("Unable to update file name")?,
			SortOrderKind::ModificationDate =>
				_ = self
					.0
					.update_modified_date()
					.context("Unable to update modified date")?,
			SortOrderKind::Size => _ = self.0.update_size().context("Unable to update size")?,
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

/// Image details for an entry
#[derive(Clone, Debug)]
pub enum ImageDetails {
	Image { size: egui::Vec2 },
	Video {},
}
