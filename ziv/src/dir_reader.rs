//! Directory reader

// TODO: Allow recursively reading all files?

// Modules
pub mod entry;
pub mod sort_order;

// Exports
pub use self::{
	entry::DirEntry,
	sort_order::{SortOrder, SortOrderKind},
};

// Imports
use {
	crate::util::AppError,
	app_error::Context,
	core::{mem, ops::IntoBounds, time::Duration},
	parking_lot::{Mutex, MutexGuard},
	std::{
		ffi::OsStr,
		fs::{self},
		path::{Path, PathBuf},
		sync::{Arc, mpsc},
		thread,
	},
	zutil_cloned::cloned,
};

/// Directory reader
///
/// This is an asynchronous (albeit non-future based) directory reader,
/// list, watcher and cursor.
///
/// # List
/// This reader keeps a list of all the files read, sorted by a dynamic
/// [sort order](SortOrder).
///
/// You may change the sort order, which will remove all entries and re-
/// insert them with the new sort order asynchronously.
///
/// # Watcher
/// This reader also watches for any changes in the directory, ensuring
/// the list stays up to date with the directory state
///
/// # Cursor
/// The list also keeps a cursor exposing both an index (based on the sort
/// order) and the entry itself.
#[derive(derive_more::Debug)]
pub struct DirReader {
	inner: Arc<Mutex<Inner>>,

	_read_thread: thread::JoinHandle<()>,
	_sort_thread: thread::JoinHandle<()>,

	sort_thread_tx: mpsc::Sender<SortOrder>,
}

impl DirReader {
	/// Creates a new directory reader
	// TODO: Not need to drill the egui context through here?
	pub fn new(path: PathBuf) -> Self {
		let inner = Arc::new(Mutex::new(Inner {
			sort_order:         SortOrder {
				reverse: false,
				kind:    SortOrderKind::FileName,
			},
			entries:            vec![],
			sort_progress:      None,
			cur_entry:          None,
			visitor:            None,
			allowed_extensions: vec![],
		}));

		#[cloned(inner)]
		let read_thread = thread::spawn(move || {
			if let Err(err) = Self::read(&inner, &path) {
				tracing::error!("Unable to read directory {path:?}: {err:?}");
			}
			tracing::info!("Reader thread finished");
		});

		let (sort_thread_tx, sort_thread_rx) = mpsc::channel();
		#[cloned(inner)]
		let sort_thread = thread::spawn(move || {
			Self::set_sort_order_inner(&inner, &sort_thread_rx);
			tracing::warn!("Sorting thread returned");
		});

		Self {
			inner,
			_read_thread: read_thread,
			_sort_thread: sort_thread,
			sort_thread_tx,
		}
	}

	/// Sets the visitor
	pub fn set_visitor(&self, visitor: impl Visitor + Send + Sync + 'static) {
		self.inner.lock().visitor = Some(Arc::new(visitor));
	}

	/// Adds allowed extensions
	pub fn add_allowed_extensions(&self, extensions: impl IntoIterator<Item = &'static str>) {
		self.inner.lock().allowed_extensions.extend(extensions);
	}

	/// Gets the sort order
	pub fn sort_order(&self) -> SortOrder {
		self.inner.lock().sort_order
	}

	/// Returns whether a sorting is currently happening, as well as progress
	pub fn sort_progress(&self) -> Option<SortProgress> {
		self.inner.lock().sort_progress.clone()
	}

	/// Sets the sort order
	pub fn set_sort_order(&self, sort_order: SortOrder) {
		_ = self.sort_thread_tx.send(sort_order);
	}

	/// Returns a range of entries
	pub fn entry_range<R>(&self, range: R) -> Option<Vec<DirEntry>>
	where
		R: IntoBounds<usize>,
	{
		let entries = self
			.inner
			.lock()
			.entries
			.get(range.into_bounds())?
			.iter()
			.map(DirEntry::clone)
			.collect();

		Some(entries)
	}

	/// Gets the current entry.
	///
	/// # Initialization
	/// When creating the directory reader, no current entry is set by default,
	/// but this method creates a few defaults.
	///
	/// If a non-directory was passed to [`Self::new`], then that will be used
	/// to initialize the current entry once the directory is read.
	///
	/// Otherwise, the first entry according to the current sort order will
	/// be used to initialize the current entry.
	///
	/// # Return type
	/// The returned type contains both the entry as well as it's index (if
	/// it's currently inserted into the list).
	///
	/// When re-sorting the list, the current entry will not be forgotten,
	/// but may lose it's index temporarily until it gets sorted.
	pub fn cur_entry(&self) -> Option<CurEntry> {
		self.inner.lock().cur_entry()
	}

	/// Removes the current entry from the list
	pub fn cur_entry_remove(&self) {
		self.inner.lock().cur_entry_remove();
	}

	/// Advances the current entry to the next one, wrapping around if the last entry is reached
	pub fn cur_entry_set_next(&self) -> Option<CurEntry> {
		self.inner.lock().cur_entry_set_next()
	}

	/// Advances the current entry to the previous one, wrapping around if the first entry is reached
	pub fn cur_entry_set_prev(&self) -> Option<CurEntry> {
		self.inner.lock().cur_entry_set_prev()
	}

	/// Advances the current entry to the first one
	pub fn cur_entry_set_first(&self) -> Option<CurEntry> {
		self.inner.lock().cur_entry_set_first()
	}

	/// Advances the current entry to the last one
	pub fn cur_entry_set_last(&self) -> Option<CurEntry> {
		self.inner.lock().cur_entry_set_last()
	}

	/// Sets the current entry to an entry
	pub fn cur_entry_set(&self, entry: DirEntry) {
		self.inner.lock().cur_entry_set(entry);
	}

	/// Returns the number of entries
	pub fn len(&self) -> usize {
		self.inner.lock().entries.len()
	}

	/// Returns the index of an entry.
	pub fn idx_of(&self, entry: &DirEntry) -> Result<Option<usize>, AppError> {
		self.inner.lock().search(entry).map(Result::ok)
	}

	/// Reads a path into this directory reader.
	///
	/// Directories will be read, while files will have their parent read
	///
	/// # Errors
	/// If unable to read the directory or any of the entries, returns `Err`.
	///
	/// If processing the entries results in an error, logs it and continues
	fn read(inner: &Arc<Mutex<Inner>>, mut path: &Path) -> Result<(), AppError> {
		let path_metadata = path.metadata().context("Unable to get path metadata")?;

		let mut begin_entry = None;
		if !path_metadata.is_dir() {
			let begin_entry_path = path.to_owned();

			// Note: If `path` is just a filename, then it's parent will be empty,
			//       but walkdir doesn't like empty paths, so we replace them with `.`.
			path = path.parent().context("Path had no parent")?;
			if path.is_empty() {
				path = Path::new(".");
			}

			let entry = Self::read_path(inner, &begin_entry_path)
				.context("Unable to read path")?
				.context("Specified path was not an image")?;
			entry.set_metadata(path_metadata);
			let entry = begin_entry.insert(entry);
			inner.lock().cur_entry = Some(CurEntry {
				entry: entry.clone(),
				idx:   None,
			});
		}

		// Before reading it, start a watcher
		// Note: We do it *before* reading to ensure we don't miss any new files
		//       added during the traversal later on.
		#[cloned(inner)]
		let event_handler = move |result: notify_debouncer_full::DebounceEventResult| match result {
			Ok(events) =>
				for notify_debouncer_full::DebouncedEvent { event, .. } in events {
					tracing::trace!("Received watch event: {event:?}");
					match event.kind {
						notify::EventKind::Create(_) =>
							for path in event.paths {
								if let Err(err) = Self::read_path(&inner, &path) {
									tracing::warn!("Unable to read path {path:?}: {err:?}");
								}
							},

						notify::EventKind::Modify(notify::event::ModifyKind::Name(notify::event::RenameMode::Both)) => {
							let mut paths = event.paths.into_iter().array_chunks();
							for [from_path, to_path] in &mut paths {
								inner.lock().rename(&from_path, to_path);
							}

							if let Some(remaining) = paths.into_remainder() &&
								!remaining.is_empty()
							{
								tracing::warn!(
									"Ignoring remaining paths in rename event: {:?}",
									remaining.collect::<Vec<_>>()
								);
							}
						},

						// TODO: `Remove` events don't seem to be emitted, just `Modify(Name(From))`,
						//       so we also remove when that happens
						notify::EventKind::Remove(_) |
						notify::EventKind::Modify(notify::event::ModifyKind::Name(notify::event::RenameMode::From)) =>
							for path in event.paths {
								inner.lock().remove(&path);
							},
						_ => tracing::trace!("Ignoring watch event"),
					}
				},
			Err(errors) =>
				for err in errors {
					tracing::warn!("Received a filesystem error while watching: {:?}", AppError::new(&err));
				},
		};

		let mut debouncer = notify_debouncer_full::new_debouncer(Duration::from_secs(1), None, event_handler)
			.context("Unable to create watch debouncer")?;
		debouncer
			.watch(path, notify::RecursiveMode::NonRecursive)
			.context("Unable to watch directory")?;

		let scan_dir = walkdir::WalkDir::new(path).min_depth(1).max_depth(1);
		for entry in scan_dir {
			let entry = entry.context("Unable to read entry")?;
			let entry_path = entry.path();

			// Skip if we're already added this one
			if let Some(begin_entry) = &begin_entry &&
				&*begin_entry.path() == entry_path
			{
				continue;
			}

			if let Err(err) = Self::read_dir_entry(inner, entry_path, &entry) {
				tracing::warn!("Unable to read directory entry {:?}: {err:?}", entry.path());
			}
		}

		// TODO: Once we implement re-reading directories, replace this with a loop over
		//       some channel.
		// Note: For now this is necessary to ensure we don't drop the directory watcher,
		//       which would stop watching
		loop {
			std::thread::park();
		}
	}

	fn read_dir_entry(
		inner: &Arc<Mutex<Inner>>,
		path: &Path,
		entry: &walkdir::DirEntry,
	) -> Result<Option<DirEntry>, AppError> {
		let file_type = entry.file_type();
		Self::read_path_with_file_type(inner, path, MetadataOrFileType::FileType(file_type))
	}

	fn read_path(inner: &Arc<Mutex<Inner>>, path: &Path) -> Result<Option<DirEntry>, AppError> {
		// TODO: Could we get away with only getting the file type in some scenarios?
		let metadata = fs::metadata(path).context("Unable to get metadata")?;
		Self::read_path_with_file_type(inner, path, MetadataOrFileType::Metadata(metadata))
	}

	fn read_path_with_file_type(
		inner: &Arc<Mutex<Inner>>,
		path: &Path,
		metadata: MetadataOrFileType,
	) -> Result<Option<DirEntry>, AppError> {
		if metadata.file_type().is_dir() {
			tracing::info!("Ignoring directory: {path:?}");
			return Ok(None);
		}

		if path
			.extension()
			.and_then(OsStr::to_str)
			.is_none_or(|ext| !inner.lock().allowed_extensions.contains(&ext))
		{
			tracing::info!("Ignoring non-image: {path:?}");
			return Ok(None);
		}

		// Create the entry and add the metadata if we already have it
		let mut inner = inner.lock();
		let entry = DirEntry::new(path.to_owned());
		if let Some(metadata) = metadata.try_into_metadata() {
			entry.set_metadata(metadata);
		}

		// Then search to where to insert it and insert it
		let (Ok(idx) | Err(idx)) = inner.search(&entry)?;
		inner.insert(idx, entry.clone());
		drop(inner);

		Ok(Some(entry))
	}

	/// Sets the sort order
	fn set_sort_order_inner(inner: &Arc<Mutex<Inner>>, rx: &mpsc::Receiver<SortOrder>) {
		let mut next_sort_order = None;
		let mut orphaned_entries = None;
		while let Some(sort_order) = next_sort_order.take().or_else(|| rx.recv().ok()) {
			// Checks whether a new sort order was issued so we can abort the current one
			let mut check_for_new_sort = || {
				if let Ok(sort_order) = rx.try_recv() {
					next_sort_order = Some(sort_order);
				}

				next_sort_order.is_some()
			};

			let entries = {
				let mut inner = inner.lock();
				inner.sort_order = sort_order;

				let mut entries = mem::take(&mut inner.entries);
				if let Some(cur_entry) = &mut inner.cur_entry {
					cur_entry.idx = None;
				}
				drop(inner);

				// Append any previously orphaned entries from a failed sort
				if let Some(orphaned_entries) = orphaned_entries.take() {
					entries.extend(orphaned_entries);
				}
				entries
			};

			let mut inner = inner.lock();
			inner.sort_progress = Some(SortProgress {
				sorted: 0,
				total:  entries.len(),
			});

			// TODO: Should we process the current item first?
			let mut entries = entries.into_iter();
			while let Some(entry) = entries.next() {
				if check_for_new_sort() {
					orphaned_entries = Some(entries);
					break;
				}

				match inner.search(&entry) {
					Ok(Ok(idx) | Err(idx)) => inner.insert(idx, entry),
					Err(err) => tracing::warn!("Unable to load entry {:?}, removing: {err:?}", entry.path()),
				}

				inner.sort_progress.as_mut().expect("We just inserted it").sorted += 1;
			}

			inner.sort_progress = None;
		}
	}
}

enum MetadataOrFileType {
	Metadata(fs::Metadata),
	FileType(fs::FileType),
}

impl MetadataOrFileType {
	/// Returns the file type of this
	pub fn file_type(&self) -> fs::FileType {
		match self {
			Self::Metadata(metadata) => metadata.file_type(),
			Self::FileType(file_type) => *file_type,
		}
	}

	/// Converts this into metadata, if available
	pub const fn try_into_metadata(self) -> Option<fs::Metadata> {
		match self {
			Self::Metadata(metadata) => Some(metadata),
			Self::FileType(_) => None,
		}
	}
}

#[derive(derive_more::Debug)]
struct Inner {
	// Invariant: All entries have the field for `sort_order` loaded.
	entries: Vec<DirEntry>,

	sort_order:    SortOrder,
	sort_progress: Option<SortProgress>,

	cur_entry: Option<CurEntry>,

	#[debug(ignore)]
	visitor: Option<Arc<dyn Visitor + Send + Sync>>,

	allowed_extensions: Vec<&'static str>,
}

impl Inner {
	/// Gets the current entry.
	///
	/// See [`DirReader::cur_entry`] for details
	pub fn cur_entry(self: &mut MutexGuard<'_, Self>) -> Option<CurEntry> {
		let entry = loop {
			// Get the current entry, or use the first one if it doesn't exist
			let cur_entry = match &self.cur_entry {
				Some(cur_entry) => cur_entry.clone(),
				None => {
					let entry = self.entries.first()?.clone();
					self.cur_entry.insert(CurEntry { entry, idx: Some(0) }).clone()
				},
			};

			// If it has an index, we can return it without any more processing
			if cur_entry.idx.is_some() {
				break cur_entry;
			}

			// Otherwise, try to search for it's index.
			// Note: On errors, we remove the current entry and try again.
			//       We only need to remove it from the field because all entries
			//       in the list are guaranteed to be loaded for the correct sort order.
			let idx = match self.search(&cur_entry.entry) {
				Ok(idx) => idx,
				Err(err) => {
					tracing::warn!("Unable to load entry {:?}, removing: {err:?}", cur_entry.path());
					self.cur_entry = None;
					continue;
				},
			};

			// If it doesn't have an index, it's being processed asynchronously, so
			// nothing we can do, just return it without an index
			let Ok(idx) = idx else {
				break cur_entry;
			};

			// Otherwise, we need to set the index.
			// However, since we might have unlocked the mutex when loading the field,
			// we first need to make sure the current entry hasn't changed. If it has,
			// we need to try again
			if let Some(cur_entry_after) = &mut self.cur_entry &&
				cur_entry_after.entry == cur_entry.entry
			{
				cur_entry_after.idx = Some(idx);
				break cur_entry_after.clone();
			}
		};

		Some(entry)
	}

	/// Removes the current entry
	pub fn cur_entry_remove(self: &mut MutexGuard<'_, Self>) {
		let Some(cur_entry) = self.cur_entry() else {
			return;
		};

		// Note: If we don't have an index, then we haven't been added
		//       to the list yet, so we can just remove ourselves.
		//       If we get added again, and the user calls remove, then
		//       we'll have an index to remove from.
		if let Some(idx) = cur_entry.idx {
			self.entries.remove(idx);
		}

		self.cur_entry = None;

		// TODO: When removed, it'd be jarring to reset to the first entry,
		//       should we just try to reset it to the previous / next entry,
		//       if they exist?
	}

	/// Advances the current entry.
	///
	/// See [`DirReader::cur_entry_set_next`] for details
	pub fn cur_entry_set_next(self: &mut MutexGuard<'_, Self>) -> Option<CurEntry> {
		// Note: If we don't have an index, we don't know what the next is
		let cur_entry = self.cur_entry()?;
		let cur_idx = cur_entry.idx?;

		let (idx, entry) = cur_idx
			.checked_add(1)
			.and_then(|idx| Some((idx, self.entries.get(idx)?)))
			.or_else(|| Some((0, self.entries.first()?)))?;

		let entry = CurEntry {
			entry: entry.clone(),
			idx:   Some(idx),
		};
		Some(self.cur_entry.insert(entry).clone())
	}

	/// Advances the current entry backwards.
	///
	/// See [`DirReader::cur_entry_set_prev`] for details
	pub fn cur_entry_set_prev(self: &mut MutexGuard<'_, Self>) -> Option<CurEntry> {
		// Note: If we don't have an index, we don't know what the previous is
		let cur_entry = self.cur_entry()?;
		let cur_idx = cur_entry.idx?;

		let (idx, entry) = cur_idx
			.checked_sub(1)
			.and_then(|idx| Some((idx, self.entries.get(idx)?)))
			.or_else(|| Some((self.entries.len().checked_sub(1)?, self.entries.last()?)))?;

		let entry = CurEntry {
			entry: entry.clone(),
			idx:   Some(idx),
		};
		Some(self.cur_entry.insert(entry).clone())
	}

	/// Sets the current entry to the first one.
	///
	/// See [`DirReader::cur_entry_set_first`] for details
	pub fn cur_entry_set_first(&mut self) -> Option<CurEntry> {
		let entry = self.first()?;
		Some(self.cur_entry.insert(CurEntry { entry, idx: Some(0) }).clone())
	}

	/// Sets the current entry to the last one.
	///
	/// See [`DirReader::cur_entry_set_last`] for details
	pub fn cur_entry_set_last(&mut self) -> Option<CurEntry> {
		let entry = self.last()?;
		Some(
			self.cur_entry
				.insert(CurEntry {
					entry,
					idx: Some(self.entries.len() - 1),
				})
				.clone(),
		)
	}

	/// Sets the current entry to a specific entry
	///
	/// See [`DirReader::cur_entry_set`] for details
	pub fn cur_entry_set(&mut self, entry: DirEntry) {
		self.cur_entry = Some(CurEntry { entry, idx: None });
	}

	/// Binary searches the index of `entry`.
	///
	/// See [`slice::binary_search`] for details on the return type.
	pub fn search(self: &mut MutexGuard<'_, Self>, entry: &DirEntry) -> Result<Result<usize, usize>, AppError> {
		loop {
			let sort_order = self.sort_order;
			MutexGuard::unlocked(self, || entry.load_for_order(sort_order))?;
			if self.sort_order == sort_order {
				break;
			}
		}

		let idx = self.entries.binary_search_by(|other_entry| {
			other_entry
				.cmp_with(entry, self.sort_order)
				.expect("Entry sort field was unloaded")
		});

		Ok(idx)
	}

	/// Gets the first entry
	pub fn first(&self) -> Option<DirEntry> {
		self.entries.first().cloned()
	}

	/// Gets the last entry
	pub fn last(&self) -> Option<DirEntry> {
		self.entries.last().cloned()
	}

	/// Renames an entry
	pub fn rename(&self, from_path: &Path, to_path: PathBuf) {
		// TODO: Both of these are `O(N)`
		let Some(entry) = self.entries.iter().find(|entry| &*entry.path() == from_path) else {
			return;
		};

		entry.rename(to_path);
	}

	/// Removes an entry by path
	pub fn remove(&mut self, path: &Path) -> Option<DirEntry> {
		// TODO: Both of these are `O(N)`
		let idx = self.entries.iter().position(|entry| &*entry.path() == path)?;
		let entry = self.entries.remove(idx);
		if let Some(cur_entry) = &self.cur_entry &&
			cur_entry.entry == entry
		{
			self.cur_entry = None;
		}

		Some(entry)
	}

	/// Inserts an entry
	pub fn insert(self: &mut MutexGuard<'_, Self>, idx: usize, entry: DirEntry) {
		// TODO: This is `O(N)`, we need a better storage
		self.entries.insert(idx, entry.clone());

		if let Some(cur_entry) = &mut self.cur_entry &&
			let Some(cur_idx) = &mut cur_entry.idx &&
			*cur_idx >= idx
		{
			*cur_idx += 1;
		}

		if let Some(visitor) = self.visitor.as_ref().map(Arc::clone) {
			MutexGuard::unlocked(self, || visitor.entry_added(entry));
		}
	}
}


#[derive(Clone, Debug)]
pub struct SortProgress {
	pub sorted: usize,
	pub total:  usize,
}

#[derive(Clone, Debug, derive_more::Deref, derive_more::Into)]
pub struct CurEntry {
	#[deref]
	#[into]
	entry:   DirEntry,
	pub idx: Option<usize>,
}

impl PartialEq<DirEntry> for CurEntry {
	fn eq(&self, other: &DirEntry) -> bool {
		&self.entry == other
	}
}

impl PartialEq<CurEntry> for DirEntry {
	fn eq(&self, other: &CurEntry) -> bool {
		self == &other.entry
	}
}

/// Visitor
pub trait Visitor {
	/// Called when a new entry was added
	fn entry_added(&self, dir_entry: DirEntry);
}
