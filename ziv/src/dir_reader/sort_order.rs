//! Sort order

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub struct SortOrder {
	pub reverse: bool,
	pub kind:    SortOrderKind,
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum SortOrderKind {
	FileName,
	ModificationDate,
}
