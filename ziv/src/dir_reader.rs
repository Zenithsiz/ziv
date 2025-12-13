//! Directory reader

// TODO: Allow recursively reading all files?

// Modules
pub mod entries;
pub mod entry;
mod read_thread;
pub mod sort_order;
mod sort_thread;

// Exports
pub use self::{
	entries::Entries,
	entry::DirEntry,
	sort_order::{SortOrder, SortOrderKind},
};

// Imports
use {
	self::{entry::EntrySource, read_thread::ReadThread, sort_thread::SortThread},
	crate::util::AppError,
	app_error::Context,
	core::ops::{Bound, IntoBounds},
	parking_lot::{Mutex, MutexGuard},
	std::{
		iter,
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
#[derive(Debug)]
pub struct DirReader {
	inner: Arc<Mutex<Inner>>,

	_read_thread: thread::JoinHandle<()>,
	_sort_thread: thread::JoinHandle<()>,

	sort_thread_tx: mpsc::Sender<SortOrder>,
}

impl DirReader {
	/// Creates a new directory reader
	pub fn new(path: PathBuf) -> Result<Self, AppError> {
		// TODO: This needs to be configurable.
		let sort_order = SortOrder {
			reverse: false,
			kind:    SortOrderKind::FileName,
		};
		let inner = Arc::new(Mutex::new(Inner {
			entries:       Entries::new(sort_order),
			sort_progress: None,
			cur_entry:     None,
			visitor:       None,
		}));

		#[cloned(inner)]
		let read_thread = thread::Builder::new()
			.name("read".to_owned())
			.spawn(move || {
				let read_thread = ReadThread::new(inner);
				match read_thread.run(&path) {
					Ok(()) => tracing::debug!("Reader thread finished"),
					Err(err) => tracing::error!("Reader thread returned an error reading {path:?}: {err:?}"),
				}
			})
			.context("Unable to spawn read thread")?;

		let (sort_thread_tx, sort_thread_rx) = mpsc::channel();
		#[cloned(inner)]
		let sort_thread = thread::Builder::new()
			.name("sort".to_owned())
			.spawn(move || {
				let sort_thread = SortThread::new(inner, sort_thread_rx);
				sort_thread.run();
				tracing::debug!("Sorting thread finished");
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
		self.inner.lock().entries.sort_order()
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

		let entries_after = inner
			.entries
			.range((Bound::Excluded(idx), Bound::Unbounded))
			.into_iter()
			.flatten();
		let entries = iter::chain(entries_after, inner.entries.iter())
			.take(len)
			.cloned()
			.collect();

		Ok(entries)
	}

	/// Returns the next `len` entries before `entry`, wrapping around
	pub fn before_entry(&self, entry: &DirEntry, len: usize) -> Result<Vec<DirEntry>, AppError> {
		let mut inner = self.inner.lock();
		let idx = inner.search(entry)?;

		let entries_before = inner
			.entries
			.range((Bound::Unbounded, Bound::Excluded(idx)))
			.into_iter()
			.flatten()
			.rev();
		let entries = iter::chain(entries_before, inner.entries.iter().rev())
			.take(len)
			.cloned()
			.collect();

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
}

#[derive(derive_more::Debug)]
struct Inner {
	entries: Entries,

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

			// If it has an index, or isn't loaded for the current sort order,
			// there's nothing we can do without blocking, so quit
			if cur_entry.idx.is_some() {
				break cur_entry;
			}
			match cur_entry.entry.is_loaded_for_order(self.entries.sort_order()) {
				Ok(is_loaded) =>
					if !is_loaded {
						break cur_entry;
					},
				Err(err) => {
					tracing::warn!(
						"Unable to load entry {:?}, removing: {err:?}",
						cur_entry.source().name()
					);
					self.cur_entry = None;
					continue;
				},
			}

			// Otherwise, try to search for it's index.
			// Note: On errors, we remove the current entry and try again.
			//       We only need to remove it from the field because all entries
			//       in the list are guaranteed to be loaded for the correct sort order.
			let idx = match self.search(&cur_entry.entry) {
				Ok(idx) => idx,
				Err(err) => {
					tracing::warn!(
						"Unable to load entry {:?}, removing: {err:?}",
						cur_entry.source().name()
					);
					self.cur_entry = None;
					continue;
				},
			};

			// Since we might have unlocked the mutex when loading the field,
			// we first need to make sure the current entry hasn't changed.
			// If it hasn't, we can just assign the index and return, otherwise
			// we repeat the loop.
			if let Some(new_cur_entry) = &mut self.cur_entry &&
				new_cur_entry.entry == cur_entry.entry
			{
				new_cur_entry.idx = Some(idx);
				break new_cur_entry.clone();
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
	// TODO: This really needs to return a `Result<usize, usize>` to avoid
	//       bugs elsewhere that assume the item exists
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
		let Some(entry) = self.entries.iter().find(|entry| {
			let EntrySource::Path(entry_path) = entry.source() else {
				return false;
			};
			&*entry_path == from_path
		}) else {
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

		if let Some(cur_entry) = &mut self.cur_entry {
			// TODO: Re-compute the index here?
			cur_entry.idx = None;

			if cur_entry.entry == *entry {
				self.cur_entry = None;
			}
		}

		Ok(true)
	}

	/// Removes an entry by path
	pub fn remove_by_path(self: &mut MutexGuard<'_, Self>, path: &Path) -> Result<Option<DirEntry>, AppError> {
		// TODO: Make finding by path not `O(N)`?
		let Some(entry) = self
			.entries
			.iter()
			.find(|entry| {
				let EntrySource::Path(entry_path) = entry.source() else {
					return false;
				};
				&*entry_path == path
			})
			.cloned()
		else {
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
			let sort_order = self.entries.sort_order();
			MutexGuard::unlocked(self, || entry.load_for_order(sort_order))?;
			if self.entries.sort_order() == sort_order {
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
