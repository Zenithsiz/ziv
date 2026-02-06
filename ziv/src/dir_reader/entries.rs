//! Entries

// Imports
use {
	super::{DirEntry, SortOrder, SortOrderKind},
	core::ops::{Bound, IntoBounds},
	ref_cast::RefCast,
	std::borrow::Borrow,
};

dir_entry_wrappers! {
	Entries;
	Inner;
	Range;
	RangeInner;
	Iter;
	IterInner;
	new;
	set_reverse;
	sort_order;
	len;
	first;
	last;
	insert;
	remove;
	get;
	search;
	range;
	iter;

	FileName,
	ModificationDate,
	Size,
	ResolutionWidth,
	ResolutionHeight,
	Random,
}

macro dir_entry_wrappers {
	(
		$Entries:ident;
		$Inner:ident;
		$Range:ident;
		$RangeInner:ident;
		$Iter:ident;
		$IterInner:ident;
		$new:ident;
		$set_reverse:ident;
		$sort_order:ident;
		$len:ident;
		$first:ident;
		$last:ident;
		$insert:ident;
		$remove:ident;
		$get:ident;
		$search:ident;
		$range:ident;
		$iter:ident;

		$(
			$SortOrderKind:ident
		),*
		$(,)?
	) => {
		#[derive(Debug)]
		enum $Inner {
			$(
				$SortOrderKind(indexset::BTreeSet<${concat(DirEntry, $SortOrderKind)}>),
			)*
		}

		/// Entries
		#[derive(Debug)]
		pub struct $Entries {
			inner:   $Inner,
			reverse: bool,
		}

		impl $Entries {
			pub fn $new(sort_order: SortOrder) -> Self {
				let inner = match sort_order.kind {
					$(
						SortOrderKind::$SortOrderKind => $Inner::$SortOrderKind(indexset::BTreeSet::new()),
					)*
				};

				Self {
					inner,
					reverse: sort_order.reverse,
				}
			}

			pub const fn $set_reverse(&mut self, reverse: bool) {
				self.reverse = reverse;
			}

			pub const fn $sort_order(&self) -> SortOrder {
				let kind = match self.inner {
					$(
						Inner::$SortOrderKind(_) => SortOrderKind::$SortOrderKind,
					)*
				};

				SortOrder {
					reverse: self.reverse,
					kind,
				}
			}

			pub fn $len(&self) -> usize {
				match &self.inner {
					$( $Inner::$SortOrderKind(entries) => entries.len(), )*
				}
			}

			pub fn $first(&self) -> Option<&DirEntry> {
				match &self.inner {
					$( $Inner::$SortOrderKind(entries) => match self.reverse {
						true => entries.last().map(AsRef::as_ref),
						false => entries.first().map(AsRef::as_ref),
					}, )*
				}
			}

			pub fn $last(&self) -> Option<&DirEntry> {
				match &self.inner {
					$( $Inner::$SortOrderKind(entries) => match self.reverse {
						true => entries.first().map(AsRef::as_ref),
						false => entries.last().map(AsRef::as_ref),
					}, )*
				}
			}

			pub fn $insert(&mut self, entry: DirEntry) -> bool {
				// Note: We only check for reverse on access, so no need to do anything here
				match &mut self.inner {
					$( $Inner::$SortOrderKind(entries) => entries.insert(entry.into()), )*
				}
			}

			pub fn $remove(&mut self, entry: &DirEntry) -> bool {
				// Note: We only check for reverse on access, so no need to do anything here
				match &mut self.inner {
					$( $Inner::$SortOrderKind(entries) => entries.remove(${concat(DirEntry, $SortOrderKind)}::ref_cast(entry)), )*
				}
			}

			pub fn $get(&self, idx: usize) -> Option<&DirEntry> {
				let idx = match self.reverse {
					true => self.$len().checked_sub(idx)?.checked_sub(1)?,
					false => idx,
				};

				match &self.inner {
					$( $Inner::$SortOrderKind(entries) => entries.get_index(idx).map(AsRef::as_ref), )*
				}
			}

			pub fn $search(&self, entry: &DirEntry) -> usize {
				let idx = match &self.inner {
					$( $Inner::$SortOrderKind(entries) => entries.rank(${concat(DirEntry, $SortOrderKind)}::ref_cast(entry)), )*
				};

				match self.reverse {
					// Note: If `idx == self.len`, this returns 0 instead, since
					//       the rank of an element lower than any in the list is 0.
					true => (self.$len() - idx).saturating_sub(1),
					false => idx,
				}
			}

			pub fn $range<R: IntoBounds<usize>>(&self, range: R) -> Option<Range<'_>> {
				let (start, end) = range.into_bounds();
				let len = self.$len();

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

				let iter = match &self.inner {
					$( $Inner::$SortOrderKind(entries) => entries.range_idx(bounds).into(), )*
				};

				Some(Range {
					iter,
					reverse: self.reverse
				})
			}

			pub fn $iter(&self) -> Iter<'_> {
				let iter = match &self.inner {
					$( $Inner::$SortOrderKind(entries) => entries.iter().into(), )*
				};

				Iter {
					iter,
					reverse: self.reverse,
				}
			}
		}

		#[derive(derive_more::From, derive_more::Debug)]
		enum $RangeInner<'a> {
			$(
				$SortOrderKind(#[debug(ignore)] indexset::Range<'a, ${concat(DirEntry, $SortOrderKind)}>),
			)*
		}

		#[derive(Debug)]
		pub struct $Range<'a> {
			iter:    $RangeInner<'a>,
			reverse: bool,
		}

		impl<'a> Iterator for Range<'a> {
			type Item = &'a DirEntry;

			fn next(&mut self) -> Option<Self::Item> {
				match &mut self.iter {
					$(
						$RangeInner::$SortOrderKind(iter) => self::iter_next(iter, self.reverse).map(AsRef::as_ref),
					)*
				}
			}
		}

		impl DoubleEndedIterator for Range<'_> {
			fn next_back(&mut self) -> Option<Self::Item> {
				match &mut self.iter {
					$(
						$RangeInner::$SortOrderKind(iter) => self::iter_next_back(iter, self.reverse).map(AsRef::as_ref),
					)*
				}
			}
		}

		#[derive(derive_more::From, derive_more::Debug)]
		enum $IterInner<'a> {
			$(
				$SortOrderKind(#[debug(ignore)] indexset::Iter<'a, ${concat(DirEntry, $SortOrderKind)}>),
			)*
		}

		#[derive(Debug)]
		pub struct $Iter<'a> {
			iter:    $IterInner<'a>,
			reverse: bool,
		}

		impl<'a> Iterator for Iter<'a> {
			type Item = &'a DirEntry;

			fn next(&mut self) -> Option<Self::Item> {
				match &mut self.iter {
					$(
						$IterInner::$SortOrderKind(iter) => self::iter_next(iter, self.reverse).map(AsRef::as_ref),
					)*
				}
			}
		}

		impl DoubleEndedIterator for Iter<'_> {
			fn next_back(&mut self) -> Option<Self::Item> {
				match &mut self.iter {
					$(
						$IterInner::$SortOrderKind(iter) => self::iter_next_back(iter, self.reverse).map(AsRef::as_ref),
					)*
				}
			}
		}

		impl IntoIterator for Entries {
			type Item = DirEntry;

			type IntoIter = impl Iterator<Item = Self::Item>;

			fn into_iter(self) -> Self::IntoIter {
				// TODO: Not collect all entries before returning
				let entries = match self.inner {
					$(
						$Inner::$SortOrderKind(entries) => {
							let iter = entries.into_iter().map(Into::into);
							match self.reverse {
								true => iter.rev().collect::<Vec<_>>(),
								false => iter.collect(),
							}
						},
					)*
				};

				entries.into_iter()
			}
		}

		$(
			#[derive(ref_cast::RefCast, derive_more::From, derive_more::Into, derive_more::AsRef, Debug)]
			#[repr(transparent)]
			struct ${concat(DirEntry, $SortOrderKind)}(DirEntry);

			impl Borrow<DirEntry> for ${concat(DirEntry, $SortOrderKind)} {
				fn borrow(&self) -> &DirEntry {
					&self.0
				}
			}

			impl PartialEq for ${concat(DirEntry, $SortOrderKind)} {
				fn eq(&self, other: &Self) -> bool {
					self.cmp(other).is_eq()
				}
			}

			impl Eq for ${concat(DirEntry, $SortOrderKind)} {}

			impl PartialOrd for ${concat(DirEntry, $SortOrderKind)} {
				fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
					Some(self.cmp(other))
				}
			}

			impl Ord for ${concat(DirEntry, $SortOrderKind)} {
				fn cmp(&self, other: &Self) -> std::cmp::Ordering {
					self.0
						.cmp_with(&other.0, SortOrder {
							reverse: false,
							kind:    super::SortOrderKind::$SortOrderKind,
						})
						.expect("Entry wasn't loaded")
				}
			}
		)*
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
