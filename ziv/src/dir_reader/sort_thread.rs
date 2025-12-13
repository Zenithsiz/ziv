//! Sort thread

// Imports
use {
	super::{DirEntry, Entries, SortOrder, SortProgress},
	core::mem,
	parking_lot::Mutex,
	std::{
		sync::{Arc, mpsc},
		vec,
	},
};

/// Sort thread
pub struct SortThread {
	inner: Arc<Mutex<super::Inner>>,
	rx:    mpsc::Receiver<SortOrder>,

	next_sort_order:  Option<SortOrder>,
	orphaned_entries: Option<vec::IntoIter<DirEntry>>,
}

impl SortThread {
	pub const fn new(inner: Arc<Mutex<super::Inner>>, rx: mpsc::Receiver<SortOrder>) -> Self {
		Self {
			inner,
			rx,
			next_sort_order: None,
			orphaned_entries: None,
		}
	}

	/// Gets the next sort order
	fn next_sort_order(&mut self) -> Option<SortOrder> {
		self.next_sort_order.take().or_else(|| self.rx.recv().ok())
	}

	/// Checks whether there's a new sort order.
	///
	/// Returns if there is one.
	fn check_for_new_sort(&mut self) -> bool {
		// Note: This is a while loop so we get the newest sort
		//       order instead of starting one and immediately
		//       stopping it after.
		while let Ok(sort_order) = self.rx.try_recv() {
			self.next_sort_order = Some(sort_order);
		}

		self.next_sort_order.is_some()
	}

	/// Sets the sort order and takes all entries from the list.
	fn take_entries(&mut self, sort_order: SortOrder) -> Vec<DirEntry> {
		let mut inner = self.inner.lock();

		match inner.entries.sort_order().kind == sort_order.kind {
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

				self.orphaned_entries.take().unwrap_or_default().collect::<Vec<_>>()
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
					.chain(self.orphaned_entries.take().unwrap_or_default())
					.collect::<Vec<_>>()
			},
		}
	}

	pub fn run(mut self) {
		while let Some(sort_order) = self.next_sort_order() {
			// Get the entries to sort
			let entries = self.take_entries(sort_order);

			self.inner.lock().sort_progress = Some(SortProgress {
				sorted: 0,
				total:  entries.len(),
			});

			// TODO: Should we process the current item first?
			let mut entries = entries.into_iter();
			while let Some(entry) = entries.next() {
				// If we have a new sort order, abort this one
				if self.check_for_new_sort() {
					self.orphaned_entries = Some(entries);
					break;
				}

				let mut inner = self.inner.lock();
				if let Err(err) = inner.insert(&entry) {
					tracing::warn!("Unable to load entry {:?}, removing: {err:?}", entry.source().name());
				}
				inner.sort_progress.as_mut().expect("We just inserted it").sorted += 1;
			}

			self.inner.lock().sort_progress = None;
		}
	}
}
