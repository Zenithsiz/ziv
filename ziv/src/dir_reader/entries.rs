//! Entries

// Imports
use {
	super::{DirEntry, SortOrder, SortOrderKind, sort_order::SortOrderResolutionDir},
	core::ops::{Bound, IntoBounds},
	ref_cast::RefCast,
	std::borrow::Borrow,
};

dir_entry_wrappers! {
	DirEntryFileName = super::SortOrderKind::FileName;
	DirEntryModifiedDate = super::SortOrderKind::ModificationDate;
	DirEntrySize = super::SortOrderKind::Size;
	DirEntryResolution<const DIR: SortOrderResolutionDir> = super::SortOrderKind::Resolution(DIR);
}

#[derive(Debug)]
enum Inner {
	FileName(indexset::BTreeSet<DirEntryFileName>),
	ModificationDate(indexset::BTreeSet<DirEntryModifiedDate>),
	Size(indexset::BTreeSet<DirEntrySize>),
	ResolutionWidth(indexset::BTreeSet<DirEntryResolution<{ SortOrderResolutionDir::Width }>>),
	ResolutionHeight(indexset::BTreeSet<DirEntryResolution<{ SortOrderResolutionDir::Height }>>),
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
			SortOrderKind::Resolution(SortOrderResolutionDir::Width) =>
				Inner::ResolutionWidth(indexset::BTreeSet::new()),
			SortOrderKind::Resolution(SortOrderResolutionDir::Height) =>
				Inner::ResolutionHeight(indexset::BTreeSet::new()),
		};

		Self {
			inner,
			reverse: sort_order.reverse,
		}
	}

	pub const fn sort_order(&self) -> SortOrder {
		let kind = match self.inner {
			Inner::FileName(_) => SortOrderKind::FileName,
			Inner::ModificationDate(_) => SortOrderKind::ModificationDate,
			Inner::Size(_) => SortOrderKind::Size,
			Inner::ResolutionWidth(_) => SortOrderKind::Resolution(SortOrderResolutionDir::Width),
			Inner::ResolutionHeight(_) => SortOrderKind::Resolution(SortOrderResolutionDir::Height),
		};

		SortOrder {
			reverse: self.reverse,
			kind,
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
			true => entries.last().map(AsRef::as_ref),
			false => entries.first().map(AsRef::as_ref),
		}}
	}

	pub fn last(&self) -> Option<&DirEntry> {
		match_inner! { entries @ &self.inner => match self.reverse {
			true => entries.first().map(AsRef::as_ref),
			false => entries.last().map(AsRef::as_ref),
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

		match_inner! { entries @ &self.inner => entries.get_index(idx).map(AsRef::as_ref) }
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

		let bounds = {
			let start = match start {
				Bound::Included(start) => start,
				Bound::Excluded(start) => start.checked_add(1)?,
				Bound::Unbounded => 0,
			};
			let end = match end {
				Bound::Included(end) => end.checked_add(1)?,
				Bound::Excluded(end) => end,
				Bound::Unbounded => len,
			};
			start..end
		};

		let bounds = match self.reverse {
			true => len.checked_sub(bounds.end)?..len.checked_sub(bounds.start)?,
			false => bounds,
		};

		// Note: `indexset` panics on `..0`, so we use the equivalent `..1`.
		let bounds = match bounds.end {
			0 => bounds.start..1,
			_ => bounds,
		};

		match_inner! { entries @ &self.inner => Some(Range { iter: entries.range_idx(bounds).into(), reverse: self.reverse }) }
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
	ResolutionWidth(#[debug(ignore)] indexset::Range<'a, DirEntryResolution<{ SortOrderResolutionDir::Width }>>),
	ResolutionHeight(#[debug(ignore)] indexset::Range<'a, DirEntryResolution<{ SortOrderResolutionDir::Height }>>),
}

#[derive(Debug)]
pub struct Range<'a> {
	iter:    RangeInner<'a>,
	reverse: bool,
}

impl<'a> Iterator for Range<'a> {
	type Item = &'a DirEntry;

	fn next(&mut self) -> Option<Self::Item> {
		macro inner($iter:ident) {
			self::iter_next($iter, self.reverse).map(AsRef::as_ref)
		}
		match &mut self.iter {
			RangeInner::FileName(iter) => inner!(iter),
			RangeInner::ModificationDate(iter) => inner!(iter),
			RangeInner::Size(iter) => inner!(iter),
			RangeInner::ResolutionWidth(iter) => inner!(iter),
			RangeInner::ResolutionHeight(iter) => inner!(iter),
		}
	}
}

impl DoubleEndedIterator for Range<'_> {
	fn next_back(&mut self) -> Option<Self::Item> {
		macro inner($iter:ident) {
			self::iter_next_back($iter, self.reverse).map(AsRef::as_ref)
		}
		match &mut self.iter {
			RangeInner::FileName(iter) => inner!(iter),
			RangeInner::ModificationDate(iter) => inner!(iter),
			RangeInner::Size(iter) => inner!(iter),
			RangeInner::ResolutionWidth(iter) => inner!(iter),
			RangeInner::ResolutionHeight(iter) => inner!(iter),
		}
	}
}

#[derive(derive_more::From, derive_more::Debug)]
enum IterInner<'a> {
	FileName(#[debug(ignore)] indexset::Iter<'a, DirEntryFileName>),
	ModificationDate(#[debug(ignore)] indexset::Iter<'a, DirEntryModifiedDate>),
	Size(#[debug(ignore)] indexset::Iter<'a, DirEntrySize>),
	ResolutionWidth(#[debug(ignore)] indexset::Iter<'a, DirEntryResolution<{ SortOrderResolutionDir::Width }>>),
	ResolutionHeight(#[debug(ignore)] indexset::Iter<'a, DirEntryResolution<{ SortOrderResolutionDir::Height }>>),
}

#[derive(Debug)]
pub struct Iter<'a> {
	iter:    IterInner<'a>,
	reverse: bool,
}

impl<'a> Iterator for Iter<'a> {
	type Item = &'a DirEntry;

	fn next(&mut self) -> Option<Self::Item> {
		macro inner($iter:ident) {
			self::iter_next($iter, self.reverse).map(AsRef::as_ref)
		}
		match &mut self.iter {
			IterInner::FileName(iter) => inner!(iter),
			IterInner::ModificationDate(iter) => inner!(iter),
			IterInner::Size(iter) => inner!(iter),
			IterInner::ResolutionWidth(iter) => inner!(iter),
			IterInner::ResolutionHeight(iter) => inner!(iter),
		}
	}
}

impl DoubleEndedIterator for Iter<'_> {
	fn next_back(&mut self) -> Option<Self::Item> {
		macro inner($iter:ident) {
			self::iter_next_back($iter, self.reverse).map(AsRef::as_ref)
		}
		match &mut self.iter {
			IterInner::FileName(iter) => inner!(iter),
			IterInner::ModificationDate(iter) => inner!(iter),
			IterInner::Size(iter) => inner!(iter),
			IterInner::ResolutionWidth(iter) => inner!(iter),
			IterInner::ResolutionHeight(iter) => inner!(iter),
		}
	}
}

impl IntoIterator for Entries {
	type Item = DirEntry;

	type IntoIter = impl Iterator<Item = Self::Item>;

	fn into_iter(self) -> Self::IntoIter {
		// TODO: Not collect all entries before returning
		let entries = match_inner! { entries @ self.inner => {
			let iter = entries.into_iter().map(Into::into);
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

fn iter_next_back<I: DoubleEndedIterator>(iter: &mut I, reverse: bool) -> Option<I::Item> {
	match reverse {
		true => iter.next(),
		false => iter.next_back(),
	}
}

macro match_inner($name:ident $(: $T:ident)? @ $inner:expr => $res:expr) {{
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
		Inner::ResolutionWidth($name) => {
			$( type $T = DirEntryResolution<{ SortOrderResolutionDir::Width }>; )?
			$res
		},
		Inner::ResolutionHeight($name) => {
			$( type $T = DirEntryResolution<{ SortOrderResolutionDir::Height }>; )?
			$res
		},
	}
}}

macro dir_entry_wrappers {
	(
		$(
			$Name:ident
			$(<
				$( const $GenericConst:ident: $GenericTy:ty )*
			>)?
			= $sort_order_kind:expr
			;
		)*
	) => {
		$(
			#[derive(ref_cast::RefCast, derive_more::From, derive_more::Into, derive_more::AsRef, Debug)]
			#[repr(transparent)]
			struct $Name $(< $( const $GenericConst: $GenericTy )* >)? (DirEntry);

			impl $(< $( const $GenericConst: $GenericTy )* >)? Borrow<DirEntry> for $Name $(< $( $GenericConst )* >)? {
				fn borrow(&self) -> &DirEntry {
					&self.0
				}
			}

			impl $(< $( const $GenericConst: $GenericTy )* >)? PartialEq for $Name $(< $( $GenericConst )* >)? {
				fn eq(&self, other: &Self) -> bool {
					self.cmp(other).is_eq()
				}
			}

			impl $(< $( const $GenericConst: $GenericTy )* >)? Eq for $Name $(< $( $GenericConst )* >)? {}

			impl $(< $( const $GenericConst: $GenericTy )* >)? PartialOrd for $Name $(< $( $GenericConst )* >)? {
				fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
					Some(self.cmp(other))
				}
			}

			impl $(< $( const $GenericConst: $GenericTy )* >)? Ord for $Name $(< $( $GenericConst )* >)? {
				fn cmp(&self, other: &Self) -> std::cmp::Ordering {
					self.0
						.cmp_with(&other.0, SortOrder {
							reverse: false,
							kind:    $sort_order_kind,
						})
						.expect("Entry wasn't loaded")
				}
			}
		)*
	}
}
