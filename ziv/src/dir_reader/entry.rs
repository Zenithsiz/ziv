//! Directory entry

// Imports
use {
	super::{SortOrder, SortOrderKind},
	crate::util::{AppError, OptionInspectNone},
	app_error::Context,
	core::{cmp::Ordering, hash::Hash},
	parking_lot::{Mutex, MutexGuard},
	std::{
		fs::{self, Metadata},
		path::{Path, PathBuf},
		sync::{Arc, OnceLock, mpsc},
		time::SystemTime,
	},
};

#[derive(Debug)]
struct Inner {
	path: Mutex<Arc<Path>>,

	// TODO: Is this a good idea?
	loader_tx: mpsc::Sender<(DirEntry, EntryLoadField)>,

	// TODO: Store the errors so we don't attempt to get some of these repeatedly
	//       in cases where the user doesn't remove the entry on error?

	// TODO: Support non-utf-8 file names
	file_name:     OnceLock<String>,
	metadata:      OnceLock<Metadata>,
	modified_date: OnceLock<SystemTime>,
	size:          OnceLock<u64>,
	// TODO: Unload the contents once the image is no longer on-screen
	contents:      Mutex<Option<Arc<[u8]>>>,
	// TODO: Also have a texture id stored here?
	image_details: Mutex<Option<ImageDetails>>,
}

impl Inner {
	fn path(&self) -> Arc<Path> {
		Arc::clone(&self.path.lock())
	}

	fn load_file_name(&self) -> Result<&String, AppError> {
		self.file_name.get_or_try_init(|| try {
			let path = self.path();
			path.file_name()
				.context("Missing file name")?
				.to_str()
				.context("File name was non-utf8")?
				.to_owned()
		})
	}

	fn load_metadata(&self) -> Result<&Metadata, AppError> {
		self.metadata.get_or_try_init(|| {
			let path = self.path();
			fs::metadata(path).context("Unable to get metadata")
		})
	}

	fn load_modified_date(&self) -> Result<&SystemTime, AppError> {
		self.modified_date.get_or_try_init(|| {
			self.load_metadata()
				.context("Unable to get metadata")?
				.modified()
				.context("Unable to get system time")
		})
	}

	fn load_size(&self) -> Result<&u64, AppError> {
		self.size.get_or_try_init(|| {
			let metadata = self.load_metadata().context("Unable to get metadata")?;
			Ok(metadata.len())
		})
	}

	fn load_contents(&self) -> Result<Arc<[u8]>, AppError> {
		let mut contents = self.contents.lock();
		match &*contents {
			Some(contents) => Ok(Arc::clone(contents)),
			None => {
				// TODO: This conversion is expensive (copies all the data), what can we improve?
				// TODO: If we had multiple loaders, this would be a race, since multiple threads
				//       would read the contents, should we add another mutex for loading?
				let new_contents = MutexGuard::unlocked(&mut contents, || {
					let path = self.path();
					fs::read(path).map(Arc::from).context("Unable to read file")
				})?;
				*contents = Some(Arc::clone(&new_contents));
				drop(contents);

				Ok(new_contents)
			},
		}
	}
}

#[derive(Clone, derive_more::Debug)]
pub struct DirEntry(Arc<Inner>);

impl DirEntry {
	/// Creates a new directory entry
	pub(super) fn new(path: impl Into<Arc<Path>>, loader_tx: mpsc::Sender<(Self, EntryLoadField)>) -> Self {
		Self(Arc::new(Inner {
			path: Mutex::new(path.into()),
			loader_tx,
			file_name: OnceLock::new(),
			metadata: OnceLock::new(),
			modified_date: OnceLock::new(),
			size: OnceLock::new(),
			contents: Mutex::new(None),
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
	pub fn size(&self) -> Option<u64> {
		self.0.size.get().copied().inspect_none(|| {
			_ = self.0.loader_tx.send((self.clone(), EntryLoadField::Size));
		})
	}

	/// Returns this image's contents
	pub fn contents(&self) -> Option<Arc<[u8]>> {
		let contents = self.0.contents.lock().as_ref().map(Arc::clone);

		contents.inspect_none(|| {
			_ = self.0.loader_tx.send((self.clone(), EntryLoadField::Contents));
		})
	}

	/// Removes this image's contents
	pub fn remove_contents(&self) {
		*self.0.contents.lock() = None;
	}

	/// Compares two directory entries according to a sort order
	pub(super) fn cmp_with(&self, other: &Self, order: SortOrder) -> Option<Ordering> {
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

	/// Loads a field
	pub(super) fn load_field(&self, field: EntryLoadField) -> Result<(), AppError> {
		match field {
			EntryLoadField::FileName => _ = self.0.load_file_name().context("Unable to load file name")?,
			EntryLoadField::Metadata => _ = self.0.load_metadata().context("Unable to load metadata")?,
			EntryLoadField::ModifiedDate => _ = self.0.load_modified_date().context("Unable to load modified date")?,
			EntryLoadField::Size => _ = self.0.load_size().context("Unable to load size")?,
			EntryLoadField::Contents => _ = self.0.load_contents().context("Unable to load size")?,
		}

		Ok(())
	}

	/// Loads the necessary fields for `order`
	pub(super) fn load_for_order(&self, order: SortOrder) -> Result<(), AppError> {
		let field = match order.kind {
			SortOrderKind::FileName => EntryLoadField::FileName,
			SortOrderKind::ModificationDate => EntryLoadField::ModifiedDate,
			SortOrderKind::Size => EntryLoadField::Size,
		};

		self.load_field(field)
	}
}

impl PartialEq for DirEntry {
	fn eq(&self, other: &Self) -> bool {
		Arc::ptr_eq(&self.0, &other.0)
	}
}

impl Eq for DirEntry {}

impl Hash for DirEntry {
	fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
		Arc::as_ptr(&self.0).hash(state);
	}
}

/// Image details for an entry
#[derive(Clone, Debug)]
pub enum ImageDetails {
	Image { size: egui::Vec2 },
	Video {},
}

/// An entry's field to load
#[derive(Clone, Copy, Debug)]
pub enum EntryLoadField {
	FileName,
	#[expect(dead_code, reason = "Nothing needs it yet")]
	Metadata,
	ModifiedDate,
	Size,
	Contents,
}
