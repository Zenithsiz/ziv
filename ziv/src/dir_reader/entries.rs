//! Entries

// Imports
use {
	super::{DirEntry, SortOrder, SortOrderKind},
	core::ops::{Bound, IntoBounds},
	ref_cast::RefCast,
	std::borrow::Borrow,
};

/// Entry using file name
#[derive(self::DirEntryWrapper, ref_cast::RefCast, derive_more::From, Debug)]
#[repr(transparent)]
struct DirEntryFileName(DirEntry);

impl Ord for DirEntryFileName {
	fn cmp(&self, other: &Self) -> std::cmp::Ordering {
		self.0
			.cmp_with(&other.0, SortOrder {
				reverse: false,
				kind:    super::SortOrderKind::FileName,
			})
			.expect("Entry wasn't loaded")
	}
}

/// Entry using modified date
#[derive(self::DirEntryWrapper, ref_cast::RefCast, derive_more::From, Debug)]
#[repr(transparent)]
struct DirEntryModifiedDate(DirEntry);

impl Ord for DirEntryModifiedDate {
	fn cmp(&self, other: &Self) -> std::cmp::Ordering {
		self.0
			.cmp_with(&other.0, SortOrder {
				reverse: false,
				kind:    super::SortOrderKind::ModificationDate,
			})
			.expect("Entry wasn't loaded")
	}
}

/// Entry using size
#[derive(self::DirEntryWrapper, ref_cast::RefCast, derive_more::From, Debug)]
#[repr(transparent)]
struct DirEntrySize(DirEntry);

impl Ord for DirEntrySize {
	fn cmp(&self, other: &Self) -> std::cmp::Ordering {
		self.0
			.cmp_with(&other.0, SortOrder {
				reverse: false,
				kind:    super::SortOrderKind::Size,
			})
			.expect("Entry wasn't loaded")
	}
}

#[derive(Debug)]
enum Inner {
	FileName(indexset::BTreeSet<DirEntryFileName>),
	ModificationDate(indexset::BTreeSet<DirEntryModifiedDate>),
	Size(indexset::BTreeSet<DirEntrySize>),
}

/// Entries
#[derive(Debug)]
pub struct Entries {
	inner:   Inner,
	reverse: bool,
}

impl Entries {
	pub fn new(sort_order: SortOrder) -> Self {
		let inner = match sort_order.kind {
			SortOrderKind::FileName => Inner::FileName(indexset::BTreeSet::new()),
			SortOrderKind::ModificationDate => Inner::ModificationDate(indexset::BTreeSet::new()),
			SortOrderKind::Size => Inner::Size(indexset::BTreeSet::new()),
		};

		Self {
			inner,
			reverse: sort_order.reverse,
		}
	}

	pub const fn set_reverse(&mut self, reverse: bool) {
		self.reverse = reverse;
	}

	pub fn len(&self) -> usize {
		match_inner! { entries @ &self.inner => entries.len() }
	}

	pub fn first(&self) -> Option<&DirEntry> {
		match_inner! { entries @ &self.inner => match self.reverse {
			true => entries.last().map(|entry| &entry.0),
			false => entries.first().map(|entry| &entry.0),
		}}
	}

	pub fn last(&self) -> Option<&DirEntry> {
		match_inner! { entries @ &self.inner => match self.reverse {
			true => entries.first().map(|entry| &entry.0),
			false => entries.last().map(|entry| &entry.0),
		}}
	}

	pub fn insert(&mut self, entry: DirEntry) -> bool {
		// Note: We only check for reverse on access, so no need to do anything here
		match_inner! { entries @ &mut self.inner => entries.insert(entry.into()) }
	}

	pub fn remove(&mut self, entry: &DirEntry) -> bool {
		// Note: We only check for reverse on access, so no need to do anything here
		match_inner! { entries: T @ &mut self.inner => entries.remove(T::ref_cast(entry)) }
	}

	pub fn get(&self, idx: usize) -> Option<&DirEntry> {
		let idx = match self.reverse {
			true => self.len().checked_sub(idx)?.checked_sub(1)?,
			false => idx,
		};

		match_inner! { entries @ &self.inner => entries.get_index(idx).map(|entry| &entry.0) }
	}

	pub fn search(&self, entry: &DirEntry) -> usize {
		let idx = match_inner! { entries: T @ &self.inner => entries.rank(T::ref_cast(entry)) };

		match self.reverse {
			// Note: If `idx == self.len`, this returns 0 instead, since
			//       the rank of an element lower than any in the list is 0.
			true => (self.len() - idx).saturating_sub(1),
			false => idx,
		}
	}

	pub fn range<R: IntoBounds<usize>>(&self, range: R) -> Option<Range<'_>> {
		let (start, end) = range.into_bounds();

		let len = self.len();
		let (start, end) = match self.reverse {
			true => {
				let new_start = match end {
					Bound::Included(idx) => Bound::Excluded(len.checked_sub(idx)?),
					Bound::Excluded(idx) => Bound::Excluded(len.checked_sub(idx)?.checked_sub(1)?),
					Bound::Unbounded => Bound::Unbounded,
				};
				let new_end = match start {
					Bound::Included(idx) => Bound::Excluded(len.checked_sub(idx)?),
					Bound::Excluded(idx) => Bound::Excluded(len.checked_sub(idx)?.checked_sub(1)?),
					Bound::Unbounded => Bound::Unbounded,
				};

				(new_start, new_end)
			},
			false => {
				let start_within_bounds = match start {
					Bound::Included(idx) => idx < len,
					Bound::Excluded(idx) => idx <= len,
					Bound::Unbounded => true,
				};
				let end_within_bounds = match end {
					Bound::Included(idx) => idx < len,
					Bound::Excluded(idx) => idx <= len,
					Bound::Unbounded => true,
				};
				if !start_within_bounds || !end_within_bounds {
					return None;
				}

				(start, end)
			},
		};

		// Note: An excluded bound of 0 triggers a crash, so we compensate
		//       by including it and then removing the element
		let (end, pop_last) = match end {
			Bound::Excluded(0) => (Bound::Included(0), true),
			_ => (end, false),
		};

		match_inner! { entries @ &self.inner => {
			let mut iter = entries.range_idx((start, end));
			if pop_last {
				iter.next_back();
			}

			Some(Range { iter: iter.into(), reverse: self.reverse })
		}}
	}

	pub fn iter(&self) -> Iter<'_> {
		let iter = match_inner! { entries @ &self.inner => entries.iter().into() };
		Iter {
			iter,
			reverse: self.reverse,
		}
	}
}

#[derive(derive_more::From, derive_more::Debug)]
enum RangeInner<'a> {
	FileName(#[debug(ignore)] indexset::Range<'a, DirEntryFileName>),
	ModificationDate(#[debug(ignore)] indexset::Range<'a, DirEntryModifiedDate>),
	Size(#[debug(ignore)] indexset::Range<'a, DirEntrySize>),
}

#[derive(Debug)]
pub struct Range<'a> {
	iter:    RangeInner<'a>,
	reverse: bool,
}

impl<'a> Iterator for Range<'a> {
	type Item = &'a DirEntry;

	fn next(&mut self) -> Option<Self::Item> {
		match &mut self.iter {
			RangeInner::FileName(iter) => self::iter_next(iter, self.reverse).map(|entry| &entry.0),
			RangeInner::ModificationDate(iter) => self::iter_next(iter, self.reverse).map(|entry| &entry.0),
			RangeInner::Size(iter) => self::iter_next(iter, self.reverse).map(|entry| &entry.0),
		}
	}
}

#[derive(derive_more::From, derive_more::Debug)]
enum IterInner<'a> {
	FileName(#[debug(ignore)] indexset::Iter<'a, DirEntryFileName>),
	ModificationDate(#[debug(ignore)] indexset::Iter<'a, DirEntryModifiedDate>),
	Size(#[debug(ignore)] indexset::Iter<'a, DirEntrySize>),
}

#[derive(Debug)]
pub struct Iter<'a> {
	iter:    IterInner<'a>,
	reverse: bool,
}

impl<'a> Iterator for Iter<'a> {
	type Item = &'a DirEntry;

	fn next(&mut self) -> Option<Self::Item> {
		match &mut self.iter {
			IterInner::FileName(iter) => self::iter_next(iter, self.reverse).map(|entry| &entry.0),
			IterInner::ModificationDate(iter) => self::iter_next(iter, self.reverse).map(|entry| &entry.0),
			IterInner::Size(iter) => self::iter_next(iter, self.reverse).map(|entry| &entry.0),
		}
	}
}

impl IntoIterator for Entries {
	type Item = DirEntry;

	type IntoIter = impl Iterator<Item = Self::Item>;

	fn into_iter(self) -> Self::IntoIter {
		// TODO: Not collect all entries before returning
		let entries = match_inner! { entries @ self.inner => {
			let iter = entries.into_iter().map(|entry| entry.0);
			match self.reverse {
				true => iter.rev().collect::<Vec<_>>(),
				false => iter.collect(),
			}
		}};

		entries.into_iter()
	}
}

fn iter_next<I: DoubleEndedIterator>(iter: &mut I, reverse: bool) -> Option<I::Item> {
	match reverse {
		true => iter.next_back(),
		false => iter.next(),
	}
}

macro match_inner($name:ident $(: $T:ident)? @ $inner:expr => $res:expr) {
	match $inner {
		Inner::FileName($name) => {
			$( type $T = DirEntryFileName; )?
			$res
		},
		Inner::ModificationDate($name) => {
			$( type $T = DirEntryModifiedDate; )?
			$res
		},
		Inner::Size($name) => {
			$( type $T = DirEntrySize; )?
			$res
		},
	}
}

macro DirEntryWrapper {
	derive() ($(#[$meta:meta])* struct $Name:ident(DirEntry);) => {
		impl Borrow<DirEntry> for $Name {
			fn borrow(&self) -> &DirEntry {
				&self.0
			}
		}

		impl PartialEq for $Name {
			fn eq(&self, other: &Self) -> bool {
				self.cmp(other).is_eq()
			}
		}

		impl Eq for $Name {}

		impl PartialOrd for $Name {
			fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
				Some(self.cmp(other))
			}
		}
	}
}
