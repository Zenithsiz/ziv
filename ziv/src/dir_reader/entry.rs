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

#[derive(derive_more::Debug)]
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
	#[debug(ignore)]
	texture:       Mutex<Option<egui::TextureHandle>>,
	texture_load:  Mutex<()>,
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

	fn load_texture(&self, egui_ctx: &egui::Context) -> Result<(), AppError> {
		let _texture_load = self.texture_load.lock();
		let mut texture = self.texture.lock();
		match &*texture {
			Some(_) => Ok(()),
			None => {
				let loaded_texture = MutexGuard::unlocked(&mut texture, || {
					let path = self.path();
					let image = image::open(&path).context("Unable to read image")?;

					let image = egui::ColorImage::from_rgba_unmultiplied(
						[image.width() as usize, image.height() as usize],
						&image.into_rgba8().into_flat_samples().samples,
					);
					let image = egui::ImageData::Color(Arc::new(image));

					// TODO: This filter should be customizable.
					let options = egui::TextureOptions::LINEAR;
					// TODO: Could we make it so we don't require an egui context here?
					let texture = egui_ctx.load_texture(path.display().to_string(), image, options);

					Ok::<_, AppError>(texture)
				})?;
				*texture = Some(loaded_texture);
				drop(texture);

				Ok(())
			},
		}
	}

	fn remove_texture(&self) {
		let _texture_load = self.texture_load.lock();
		let mut texture = self.texture.lock();
		*texture = None;
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
			texture: Mutex::new(None),
			texture_load: Mutex::new(()),
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

	/// Returns this image's texture
	pub fn texture(&self) -> Option<egui::TextureHandle> {
		let texture = self.0.texture.lock().as_ref().cloned();

		texture.inspect_none(|| {
			_ = self.0.loader_tx.send((self.clone(), EntryLoadField::Texture));
		})
	}

	/// Removes this image's texture
	pub fn remove_texture(&self) {
		// Note: Even if it's `None` right now, we still need to send it to
		//       the loader because there might be an ongoing loading.
		_ = self.0.loader_tx.send((self.clone(), EntryLoadField::RemoveTexture));
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
	pub(super) fn load_field(&self, egui_ctx: &egui::Context, field: EntryLoadField) -> Result<(), AppError> {
		match field {
			EntryLoadField::FileName => _ = self.0.load_file_name().context("Unable to load file name")?,
			EntryLoadField::Metadata => _ = self.0.load_metadata().context("Unable to load metadata")?,
			EntryLoadField::ModifiedDate => _ = self.0.load_modified_date().context("Unable to load modified date")?,
			EntryLoadField::Size => _ = self.0.load_size().context("Unable to load size")?,
			EntryLoadField::Texture => self.0.load_texture(egui_ctx).context("Unable to load texture")?,
			EntryLoadField::RemoveTexture => self.0.remove_texture(),
		}

		Ok(())
	}

	/// Loads the necessary fields for `order`
	pub(super) fn load_for_order(&self, order: SortOrder) -> Result<(), AppError> {
		match order.kind {
			SortOrderKind::FileName => _ = self.0.load_file_name().context("Unable to load file name")?,
			SortOrderKind::ModificationDate =>
				_ = self.0.load_modified_date().context("Unable to load modified date")?,
			SortOrderKind::Size => _ = self.0.load_size().context("Unable to load size")?,
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
#[expect(dead_code, reason = "Nothing needs them yet")]
pub enum EntryLoadField {
	FileName,
	Metadata,
	ModifiedDate,
	Size,
	Texture,
	// TODO: This isn't ideal, we should maybe just put
	//       texture loading into it's own thread/thread-pool/task
	//       that we can cancel instead of this.
	RemoveTexture,
}
