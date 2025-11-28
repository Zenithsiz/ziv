//! Directory entry

// Imports
use {
	super::{SortOrder, SortOrderKind},
	crate::util::{AppError, OptionGetOrTryInsert},
	app_error::Context,
	core::cmp::Ordering,
	eframe::egui::mutex::Mutex,
	std::{
		fs::{self, Metadata},
		path::PathBuf,
		sync::Arc,
		time::SystemTime,
	},
};

#[derive(Debug)]
struct Inner {
	path:          PathBuf,
	// TODO: Support non-utf-8 file names
	file_name:     Option<String>,
	metadata:      Option<Metadata>,
	modified_date: Option<SystemTime>,
}

impl Inner {
	fn update_file_name(&mut self) -> Result<&mut String, AppError> {
		self.file_name.get_or_try_insert_with(|| try {
			self.path
				.file_name()
				.context("Missing file name")?
				.to_str()
				.context("File name was non-utf8")?
				.to_owned()
		})
	}

	fn update_metadata(&mut self) -> Result<&mut Metadata, AppError> {
		self.metadata
			.get_or_try_insert_with(|| fs::metadata(&self.path).context("Unable to get metadata"))
	}

	fn update_modified_date(&mut self) -> Result<&mut SystemTime, AppError> {
		self.update_metadata().context("Unable to get metadata")?;

		self.modified_date.get_or_try_insert_with(|| {
			self.metadata
				.as_ref()
				.expect("Just initialized")
				.modified()
				.context("Unable to get system time")
		})
	}
}

#[derive(Clone, derive_more::Debug)]
pub struct DirEntry(#[debug(ignore)] Arc<Mutex<Inner>>);

impl DirEntry {
	/// Creates a new directory entry
	pub fn new(path: PathBuf) -> Self {
		Self(Arc::new(Mutex::new(Inner {
			path,
			file_name: None,
			metadata: None,
			modified_date: None,
		})))
	}

	/// Returns this entry's path
	pub fn path(&self) -> PathBuf {
		self.0.lock().path.clone()
	}

	/// Renames this entry
	pub fn rename(&self, path: PathBuf) {
		self.0.lock().path = path;
	}

	/// Sets the metadata of this entry
	pub fn set_metadata(&self, metadata: fs::Metadata) {
		self.0.lock().metadata = Some(metadata);
	}

	/// Compares two directory entries according to a sort order
	pub fn cmp_with(&self, other: &Self, order: SortOrder) -> Option<Ordering> {
		// TODO: This isn't foolproof unfortunately, we should do
		//       something about it
		let lhs_ptr = Arc::as_ptr(&self.0);
		let rhs_ptr = Arc::as_ptr(&other.0);

		let lhs;
		let rhs;
		match lhs_ptr.cmp(&rhs_ptr) {
			Ordering::Less => {
				lhs = self.0.lock();
				rhs = other.0.lock();
			},
			Ordering::Greater => {
				rhs = other.0.lock();
				lhs = self.0.lock();
			},
			Ordering::Equal => return Some(Ordering::Equal),
		}

		let cmp = match order.kind {
			SortOrderKind::FileName => natord::compare(lhs.file_name.as_ref()?, rhs.file_name.as_ref()?),
			SortOrderKind::ModificationDate =>
				SystemTime::cmp(lhs.modified_date.as_ref()?, rhs.modified_date.as_ref()?),
		};

		match order.reverse {
			true => Some(cmp.reverse()),
			false => Some(cmp),
		}
	}

	/// Loads the necessary fields for `order`
	pub(super) fn load_for_order(&self, order: SortOrder) -> Result<(), AppError> {
		let mut inner = self.0.lock();
		match order.kind {
			SortOrderKind::FileName => _ = inner.update_file_name().context("Unable to get file name")?,
			SortOrderKind::ModificationDate =>
				_ = inner.update_modified_date().context("Unable to get modified date")?,
		}
		drop(inner);

		Ok(())
	}
}

impl PartialEq for DirEntry {
	fn eq(&self, other: &Self) -> bool {
		Arc::ptr_eq(&self.0, &other.0)
	}
}

impl Eq for DirEntry {}
