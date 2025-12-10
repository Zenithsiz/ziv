//! Directory reader

// TODO: Allow recursively reading all files?

// Modules
pub mod entries;
pub mod entry;
pub mod sort_order;

// Exports
pub use self::{
	entries::Entries,
	entry::DirEntry,
	sort_order::{SortOrder, SortOrderKind},
};

// Imports
use {
	crate::util::AppError,
	app_error::Context,
	core::{
		mem,
		ops::{Bound, IntoBounds},
		time::Duration,
	},
	parking_lot::{Mutex, MutexGuard},
	std::{
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
	pub fn new(path: PathBuf) -> Result<Self, AppError> {
		let sort_order = SortOrder {
			reverse: false,
			kind:    SortOrderKind::FileName,
		};
		let inner = Arc::new(Mutex::new(Inner {
			sort_order,
			entries: Entries::new(sort_order),
			sort_progress: None,
			cur_entry: None,
			visitor: None,
		}));

		#[cloned(inner)]
		let read_thread = thread::Builder::new()
			.name("read".to_owned())
			.spawn(move || {
				if let Err(err) = Self::read(&inner, &path) {
					tracing::error!("Unable to read directory {path:?}: {err:?}");
				}
				tracing::info!("Reader thread finished");
			})
			.context("Unable to spawn read thread")?;

		let (sort_thread_tx, sort_thread_rx) = mpsc::channel();
		#[cloned(inner)]
		let sort_thread = thread::Builder::new()
			.name("sort".to_owned())
			.spawn(move || {
				Self::set_sort_order_inner(&inner, &sort_thread_rx);
				tracing::warn!("Sorting thread returned");
			})
			.context("Unable to spawn read thread")?;

		Ok(Self {
			inner,
			_read_thread: read_thread,
			_sort_thread: sort_thread,
			sort_thread_tx,
		})
	}

	/// Sets the visitor
	pub fn set_visitor(&self, visitor: impl Visitor + Send + Sync + 'static) {
		self.inner.lock().visitor = Some(Arc::new(visitor));
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
		let entries = self.inner.lock().entries.range(range.into_bounds())?.cloned().collect();
		Some(entries)
	}

	/// Returns the next `len` entries after `entry`, wrapping around
	pub fn after_entry(&self, entry: &DirEntry, len: usize) -> Result<Vec<DirEntry>, AppError> {
		let mut inner = self.inner.lock();
		let idx = inner.search(entry)?;

		let entries = inner
			.entries
			.range((Bound::Excluded(idx), Bound::Unbounded))
			.expect("Range should be valid")
			.chain(inner.entries.iter())
			.take(len)
			.cloned()
			.collect();
		drop(inner);

		Ok(entries)
	}

	/// Returns the next `len` entries before `entry`, wrapping around
	pub fn before_entry(&self, entry: &DirEntry, len: usize) -> Result<Vec<DirEntry>, AppError> {
		let mut inner = self.inner.lock();
		let idx = inner.search(entry)?;

		let entries = inner
			.entries
			.range((Bound::Unbounded, Bound::Excluded(idx)))
			.expect("Range should be valid")
			.rev()
			.chain(inner.entries.iter().rev())
			.take(len)
			.cloned()
			.collect();
		drop(inner);

		Ok(entries)
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

	/// Returns whether a current entry exists
	pub fn has_cur_entry(&self) -> bool {
		self.inner.lock().cur_entry.is_some()
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

	/// Removes an entry from the list
	pub fn remove(&self, entry: &DirEntry) -> Result<(), AppError> {
		// TODO: Expose whether it existed or not?
		self.inner.lock().remove(entry).map(|_| ())
	}

	/// Returns the number of entries
	pub fn len(&self) -> usize {
		self.inner.lock().entries.len()
	}

	/// Returns the index of an entry.
	pub fn idx_of(&self, entry: &DirEntry) -> Result<usize, AppError> {
		self.inner.lock().search(entry)
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
								let res = inner.lock().remove_by_path(&path);
								if let Err(err) = res {
									tracing::warn!("Unable to remove entry {path:?}: {err:?}");
								}
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

			if let Err(err) = Self::read_dir_entry(inner, entry_path) {
				tracing::warn!("Unable to read directory entry {entry_path:?}: {err:?}");
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

	fn read_dir_entry(inner: &Arc<Mutex<Inner>>, path: &Path) -> Result<Option<DirEntry>, AppError> {
		Self::read_path_with_file_type(inner, path)
	}

	fn read_path(inner: &Arc<Mutex<Inner>>, path: &Path) -> Result<Option<DirEntry>, AppError> {
		Self::read_path_with_file_type(inner, path)
	}

	fn read_path_with_file_type(inner: &Arc<Mutex<Inner>>, path: &Path) -> Result<Option<DirEntry>, AppError> {
		// Create the entry and insert it
		let entry = DirEntry::new(path.to_owned());
		inner.lock().insert(&entry)?;

		Ok(Some(entry))
	}

	/// Sets the sort order
	fn set_sort_order_inner(inner: &Arc<Mutex<Inner>>, rx: &mpsc::Receiver<SortOrder>) {
		let mut next_sort_order = None;
		let mut orphaned_entries = None::<std::vec::IntoIter<_>>;
		while let Some(sort_order) = next_sort_order.take().or_else(|| rx.recv().ok()) {
			// Checks whether a new sort order was issued so we can abort the current one
			let mut check_for_new_sort = || {
				if let Ok(sort_order) = rx.try_recv() {
					next_sort_order = Some(sort_order);
				}

				next_sort_order.is_some()
			};

			// Get the entries to sort
			let entries = {
				let mut inner = inner.lock();
				let prev_sort_order = inner.sort_order;
				inner.sort_order = sort_order;

				match prev_sort_order.kind == sort_order.kind {
					// If we're just reversing the entries, set it on the entries and
					// then just sort any orphaned entries from a previous sort attempt.
					// Note: We can't just insert these entries, because they might not
					//       be loaded for the current sort order, since they could have
					//       been orphaned while sorting something else.
					true => {
						inner.entries.set_reverse(sort_order.reverse);
						// TODO: Compute the index when reversing?
						if let Some(cur_entry) = &mut inner.cur_entry {
							cur_entry.idx = None;
						}
						drop(inner);

						orphaned_entries.take().unwrap_or_default().collect::<Vec<_>>()
					},

					// Otherwise, take all the entries, and remove the index on the current one
					false => {
						let entries = mem::replace(&mut inner.entries, Entries::new(sort_order));
						if let Some(cur_entry) = &mut inner.cur_entry {
							cur_entry.idx = None;
						}
						drop(inner);

						entries
							.into_iter()
							.chain(orphaned_entries.take().unwrap_or_default())
							.collect::<Vec<_>>()
					},
				}
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

				if let Err(err) = inner.insert(&entry) {
					tracing::warn!("Unable to load entry {:?}, removing: {err:?}", entry.path());
				}

				inner.sort_progress.as_mut().expect("We just inserted it").sorted += 1;
			}

			inner.sort_progress = None;
		}
	}
}

#[derive(derive_more::Debug)]
struct Inner {
	entries: Entries,

	sort_order:    SortOrder,
	sort_progress: Option<SortProgress>,

	cur_entry: Option<CurEntry>,

	#[debug(ignore)]
	visitor: Option<Arc<dyn Visitor + Send + Sync>>,
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
			// TODO: This *can* block when sorting, so we should just return if the field
			//       for the current sort order isn't loaded.
			let idx = match self.search(&cur_entry.entry) {
				Ok(idx) => idx,
				Err(err) => {
					tracing::warn!("Unable to load entry {:?}, removing: {err:?}", cur_entry.path());
					self.cur_entry = None;
					continue;
				},
			};

			// Since we might have unlocked the mutex when loading the field,
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
		// TODO: Should we ensure the entry is loaded before setting it as the current entry?
		self.cur_entry = Some(CurEntry { entry, idx: None });
	}

	/// Binary searches the index of `entry`.
	pub fn search(self: &mut MutexGuard<'_, Self>, entry: &DirEntry) -> Result<usize, AppError> {
		self.load(entry)?;
		let idx = self.entries.search(entry);

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
		// TODO: This is `O(N)`
		let Some(entry) = self.entries.iter().find(|entry| &*entry.path() == from_path) else {
			return;
		};

		entry.rename(to_path);
	}

	/// Removes an entry
	pub fn remove(self: &mut MutexGuard<'_, Self>, entry: &DirEntry) -> Result<bool, AppError> {
		self.load(entry)?;
		if !self.entries.remove(entry) {
			return Ok(false);
		}

		if let Some(cur_entry) = &self.cur_entry &&
			cur_entry.entry == *entry
		{
			self.cur_entry = None;
		}

		Ok(true)
	}

	/// Removes an entry by path
	pub fn remove_by_path(self: &mut MutexGuard<'_, Self>, path: &Path) -> Result<Option<DirEntry>, AppError> {
		// TODO: Make finding by path not `O(N)`?
		let Some(entry) = self.entries.iter().find(|entry| &*entry.path() == path).cloned() else {
			return Ok(None);
		};
		assert!(self.remove(&entry)?);

		Ok(Some(entry))
	}

	/// Inserts an entry
	pub fn insert(self: &mut MutexGuard<'_, Self>, entry: &DirEntry) -> Result<(), AppError> {
		self.load(entry)?;
		self.entries.insert(entry.clone());

		// TODO: Update the index instead of discarding it?
		if let Some(cur_entry) = &mut self.cur_entry {
			cur_entry.idx = None;
		}

		if let Some(visitor) = self.visitor.as_ref().map(Arc::clone) {
			MutexGuard::unlocked(self, || visitor.entry_added(entry));
		}

		Ok(())
	}

	/// Loads an entry for the current sort order
	pub fn load(self: &mut MutexGuard<'_, Self>, entry: &DirEntry) -> Result<(), AppError> {
		loop {
			let sort_order = self.sort_order;
			MutexGuard::unlocked(self, || entry.load_for_order(sort_order))?;
			if self.sort_order == sort_order {
				break;
			}
		}

		Ok(())
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
	fn entry_added(&self, dir_entry: &DirEntry);
}
