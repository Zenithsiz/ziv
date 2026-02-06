//! Sort order

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub struct SortOrder {
	pub reverse: bool,
	pub kind:    SortOrderKind,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash, Debug)]
#[derive(serde::Serialize, serde::Deserialize)]
#[derive(strum::VariantArray)]
pub enum SortOrderKind {
	FileName,
	ModificationDate,
	Size,
	ResolutionWidth,
	ResolutionHeight,
	Random,
}
