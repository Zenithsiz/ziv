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
		match_inner! { entries @ &self.inner => entries.get_index(self.transform_idx(idx)).map(|entry| &entry.0) }
	}

	pub fn search(&self, entry: &DirEntry) -> usize {
		let idx = match_inner! { entries: T @ &self.inner => entries.rank(T::ref_cast(entry)) };

		self.transform_idx(idx)
	}

	pub fn range<R: IntoBounds<usize>>(&self, range: R) -> Range<'_> {
		let (start, end) = range.into_bounds();

		let (start, mut end) = match self.reverse {
			// Note: They're swapped because reversing swaps the start and end
			true => (
				end.map(|idx| self.transform_idx(idx)),
				start.map(|idx| self.transform_idx(idx)),
			),
			false => (start, end),
		};

		// Note: An excluded bound of 0 triggers a crash, so we compensate
		//       by including it and then removing the element
		let pop_last = match end {
			Bound::Excluded(0) => {
				end = Bound::Included(0);
				true
			},
			_ => false,
		};

		match_inner! { entries @ &self.inner => {
			let mut iter = 	entries.range_idx((start, end));
			if pop_last {
				iter.next_back();
			}

			Range(iter.into())
		}}
	}

	pub fn iter(&self) -> Iter<'_> {
		let iter = match_inner! { entries @ &self.inner => entries.iter().into() };
		Iter(iter)
	}

	/// Transforms an index, depending on whether we're reversed or not.
	fn transform_idx(&self, idx: usize) -> usize {
		match self.reverse {
			true => match self.len() {
				0 => 0,
				len => len - idx - 1,
			},
			false => idx,
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
pub struct Range<'a>(RangeInner<'a>);

impl<'a> Iterator for Range<'a> {
	type Item = &'a DirEntry;

	fn next(&mut self) -> Option<Self::Item> {
		match &mut self.0 {
			RangeInner::FileName(iter) => iter.next().map(|entry| &entry.0),
			RangeInner::ModificationDate(iter) => iter.next().map(|entry| &entry.0),
			RangeInner::Size(iter) => iter.next().map(|entry| &entry.0),
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
pub struct Iter<'a>(IterInner<'a>);

impl<'a> Iterator for Iter<'a> {
	type Item = &'a DirEntry;

	fn next(&mut self) -> Option<Self::Item> {
		match &mut self.0 {
			IterInner::FileName(iter) => iter.next().map(|entry| &entry.0),
			IterInner::ModificationDate(iter) => iter.next().map(|entry| &entry.0),
			IterInner::Size(iter) => iter.next().map(|entry| &entry.0),
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
