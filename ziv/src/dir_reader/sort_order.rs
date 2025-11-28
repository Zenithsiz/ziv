//! Sort order

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub struct SortOrder {
	pub reverse: bool,
	pub kind:    SortOrderKind,
}

impl SortOrder {
	/// All kinds
	pub const KINDS: [SortOrderKind; 3] = [
		SortOrderKind::FileName,
		SortOrderKind::ModificationDate,
		SortOrderKind::Size,
	];
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum SortOrderKind {
	FileName,
	ModificationDate,
	Size,
}
