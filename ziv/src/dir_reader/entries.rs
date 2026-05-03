//! Entries

// Imports
use {
	super::{DirEntry, SortOrder, SortOrderKind},
	crate::util::{AppError, PartialEqOrd},
	app_error::Context,
	core::ops::{Bound, IntoBounds},
	std::{path::PathBuf, random, time::SystemTime},
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

	FileName(SortOrderFileName),
	ModificationDate(SortOrderModificationDate),
	Size(SortOrderSize),
	ResolutionWidth(SortOrderResolutionWidth),
	ResolutionHeight(SortOrderResolutionHeight),
	Random(SortOrderRandom),
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
			$SortOrderKind:ident($SortOrderTy:ty)
		),*
		$(,)?
	) => {
		#[derive(Debug)]
		enum $Inner {
			$(
				$SortOrderKind(indexset::BTreeMap<$SortOrderTy, DirEntry>),
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
						SortOrderKind::$SortOrderKind => $Inner::$SortOrderKind(indexset::BTreeMap::new()),
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
						true => entries.last_key_value().map(|(_, entry)| entry),
						false => entries.first_key_value().map(|(_, entry)| entry),
					}, )*
				}
			}

			pub fn $last(&self) -> Option<&DirEntry> {
				match &self.inner {
					$( $Inner::$SortOrderKind(entries) => match self.reverse {
						true => entries.first_key_value().map(|(_, entry)| entry),
						false => entries.last_key_value().map(|(_, entry)| entry),
					}, )*
				}
			}

			pub fn $insert(&mut self, entry: DirEntry) -> Result<Option<DirEntry>, AppError> {
				// Note: We only check for reverse on access, so no need to do anything here
				match &mut self.inner {
					$(
						$Inner::$SortOrderKind(entries) => {
							let key = <$SortOrderTy>::new(&entry).context("Entry key was unloaded")?;
							Ok(entries.insert(key, entry))
						},
					)*
				}
			}

			pub fn $remove(&mut self, entry: &DirEntry) -> Result<Option<DirEntry>, AppError> {
				// Note: We only check for reverse on access, so no need to do anything here
				match &mut self.inner {
					$(
						$Inner::$SortOrderKind(entries) => {
							let key = <$SortOrderTy>::new(entry).context("Entry key was unloaded")?;
							Ok(entries.remove(&key))
						},
					)*
				}
			}

			pub fn $get(&self, idx: usize) -> Option<&DirEntry> {
				let idx = match self.reverse {
					true => self.$len().checked_sub(idx)?.checked_sub(1)?,
					false => idx,
				};

				match &self.inner {
					$( $Inner::$SortOrderKind(entries) => entries.get_index(idx).map(|(_, entry)| entry), )*
				}
			}

			pub fn $search(&self, entry: &DirEntry) -> Result<usize, AppError> {
				let idx = match &self.inner {
					$(
						$Inner::$SortOrderKind(entries) => {
							let key = <$SortOrderTy>::new(entry).context("Entry key was unloaded")?;
							entries.rank(&key)
						},
					)*
				};

				let idx = match self.reverse {
					// Note: If `idx == self.len`, this returns 0 instead, since
					//       the rank of an element lower than any in the list is 0.
					true => (self.$len() - idx).saturating_sub(1),
					false => idx,
				};

				Ok(idx)
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
				$SortOrderKind(#[debug(ignore)] indexset::RangeMap<'a, $SortOrderTy, DirEntry>),
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
						$RangeInner::$SortOrderKind(iter) => self::iter_next(iter, self.reverse).map(|(_, entry)| entry),
					)*
				}
			}
		}

		impl DoubleEndedIterator for Range<'_> {
			fn next_back(&mut self) -> Option<Self::Item> {
				match &mut self.iter {
					$(
						$RangeInner::$SortOrderKind(iter) => self::iter_next_back(iter, self.reverse).map(|(_, entry)| entry),
					)*
				}
			}
		}

		#[derive(derive_more::From, derive_more::Debug)]
		enum $IterInner<'a> {
			$(
				$SortOrderKind(#[debug(ignore)] indexset::IterMap<'a, $SortOrderTy, DirEntry>),
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
						$IterInner::$SortOrderKind(iter) => self::iter_next(iter, self.reverse).map(|(_, entry)| entry),
					)*
				}
			}
		}

		impl DoubleEndedIterator for Iter<'_> {
			fn next_back(&mut self) -> Option<Self::Item> {
				match &mut self.iter {
					$(
						$IterInner::$SortOrderKind(iter) => self::iter_next_back(iter, self.reverse).map(|(_, entry)| entry),
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
							let iter = entries.into_values();
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
	}
}


// TODO: Once we actually show directories, we need to ensure
//       all of these sort directories before other files.

// TODO: These shouldn't panic, and should instead return an error
//       on missing data.

#[derive(PartialEqOrd, Debug)]
struct SortOrderFileName(PathBuf);

impl Ord for SortOrderFileName {
	fn cmp(&self, other: &Self) -> core::cmp::Ordering {
		natord::compare_iter(
			self.0.as_os_str().as_encoded_bytes().iter(),
			other.0.as_os_str().as_encoded_bytes().iter(),
			|&c| c.is_ascii_whitespace(),
			|&l, &r| l.cmp(r),
			|&c| c.is_ascii_digit().then(|| isize::from(c - b'0')),
		)
	}
}

impl SortOrderFileName {
	fn new(entry: &DirEntry) -> Result<Self, AppError> {
		let file_name = entry.file_name().context("Unable to get file name")?;

		Ok(Self(file_name))
	}
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
struct SortOrderModificationDate(SystemTime);

impl SortOrderModificationDate {
	fn new(entry: &DirEntry) -> Result<Self, AppError> {
		let modified_date = entry.modified_date_if_loaded()?.context("Missing modified date")?;

		Ok(Self(modified_date))
	}
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
struct SortOrderSize(u64);

impl SortOrderSize {
	fn new(entry: &DirEntry) -> Result<Self, AppError> {
		let size = entry.size_if_loaded()?.context("Missing size")?;

		Ok(Self(size))
	}
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
struct SortOrderResolutionWidth(usize);

impl SortOrderResolutionWidth {
	fn new(entry: &DirEntry) -> Result<Self, AppError> {
		let resolution = entry.resolution_if_loaded()?.context("Missing resolution")?;

		Ok(Self(resolution.width))
	}
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
struct SortOrderResolutionHeight(usize);

impl SortOrderResolutionHeight {
	fn new(entry: &DirEntry) -> Result<Self, AppError> {
		let resolution = entry.resolution_if_loaded()?.context("Missing resolution")?;

		Ok(Self(resolution.height))
	}
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
struct SortOrderRandom(u64);

impl SortOrderRandom {
	#[expect(
		clippy::unnecessary_wraps,
		reason = "We want all sort orders to have the same signature"
	)]
	fn new(_entry: &DirEntry) -> Result<Self, AppError> {
		let random = random::random(..);

		Ok(Self(random))
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
