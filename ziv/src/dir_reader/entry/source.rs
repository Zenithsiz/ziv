//! Entry source

use {
	super::EntryMetadata,
	crate::util::AppError,
	app_error::Context,
	core::cmp::Ordering,
	parking_lot::Mutex,
	std::{
		fs,
		io::Read,
		path::{Path, PathBuf},
		sync::Arc,
	},
	zip::{ZipArchive, read::ZipFile},
};

/// Entry source
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Debug)]
pub enum EntrySource {
	/// Filesystem path
	Path(Arc<Path>),

	/// Zip file
	Zip(Arc<EntrySourceZip>),
}

impl EntrySource {
	/// Returns a name for this entry
	pub fn name(&self) -> String {
		match self {
			Self::Path(path) => path.display().to_string(),
			Self::Zip(zip) => zip.archive_path.join(&zip.file_name).display().to_string(),
		}
	}
}

/// Entry source from a zip file
#[derive(Clone, Debug)]
pub struct EntrySourceZip {
	// Note: This is the path of the zip file that we're contained in.
	archive_path: Arc<Path>,

	file_name: PathBuf,
	metadata:  EntryMetadata,
	idx:       usize,

	// TODO: Should we move this elsewhere to avoid every entry storing it?
	archive: Arc<Mutex<ZipArchive<fs::File>>>,
}

impl PartialEq for EntrySourceZip {
	fn eq(&self, other: &Self) -> bool {
		self.archive_path == other.archive_path
	}
}

impl Eq for EntrySourceZip {}

impl PartialOrd for EntrySourceZip {
	fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
		Some(self.cmp(other))
	}
}

impl Ord for EntrySourceZip {
	fn cmp(&self, other: &Self) -> Ordering {
		self.archive_path.cmp(&other.archive_path)
	}
}

impl EntrySourceZip {
	/// Creates a new zip source
	pub const fn new(
		zip_path: Arc<Path>,
		file_name: PathBuf,
		metadata: EntryMetadata,
		idx: usize,
		archive: Arc<Mutex<ZipArchive<fs::File>>>,
	) -> Self {
		Self {
			archive_path: zip_path,
			file_name,
			metadata,
			idx,
			archive,
		}
	}

	/// Accesses this file.
	pub fn try_with_file<O>(
		&self,
		f: impl FnOnce(ZipFile<'_, fs::File>) -> Result<O, AppError>,
	) -> Result<O, AppError> {
		let mut archive = self.archive.lock();
		let file = archive.by_index(self.idx).context("Unable to get zip file")?;
		f(file)
	}

	/// Reads the contents of this entry
	pub fn contents(&self) -> Result<Vec<u8>, AppError> {
		self.try_with_file(|mut file| {
			let mut contents = Vec::with_capacity(file.size() as usize);
			file.read_to_end(&mut contents).context("Unable to read zip file")?;

			Ok(contents)
		})
	}

	/// Returns if the entry is a directory
	pub fn is_dir(&self) -> Result<bool, AppError> {
		self.try_with_file(|file| Ok(file.is_dir()))
	}

	/// Returns this entry's archive path
	pub fn archive_path(&self) -> &Path {
		&self.archive_path
	}

	/// Returns the entry's file name
	pub fn file_name(&self) -> &Path {
		&self.file_name
	}

	/// Returns the entry's metadata
	pub const fn metadata(&self) -> &EntryMetadata {
		&self.metadata
	}
}
