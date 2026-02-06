//! Sort order

// Imports
use {std::marker::ConstParamTy, strum::VariantArray};

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub struct SortOrder {
	pub reverse: bool,
	pub kind:    SortOrderKind,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash, Debug)]
#[derive(strum::EnumDiscriminants)]
#[strum_discriminants(name(SortOrderKindInner))]
#[strum_discriminants(derive(strum::VariantArray))]
pub enum SortOrderKind {
	FileName,
	ModificationDate,
	Size,
	Resolution(SortOrderResolutionDir),
}

impl SortOrderKind {
	pub gen fn variants() -> Self {
		for inner in SortOrderKindInner::VARIANTS {
			match inner {
				SortOrderKindInner::FileName => yield Self::FileName,
				SortOrderKindInner::ModificationDate => yield Self::ModificationDate,
				SortOrderKindInner::Size => yield Self::Size,
				SortOrderKindInner::Resolution =>
					for &dir in SortOrderResolutionDir::VARIANTS {
						yield Self::Resolution(dir);
					},
			}
		}

		#[cfg(rust_analyzer)]
		unreachable!();
	}
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash, Debug)]
#[derive(ConstParamTy)]
#[derive(strum::VariantArray)]
pub enum SortOrderResolutionDir {
	Width,
	Height,
}

#[derive(Clone, Debug)]
#[derive(serde::Serialize, serde::Deserialize)]
enum SortOrderKindRepr {
	FileName,
	ModificationDate,
	Size,
	ResolutionWidth,
	ResolutionHeight,
}

impl serde::Serialize for SortOrderKind {
	fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
	where
		S: serde::Serializer,
	{
		let repr = match self {
			Self::FileName => SortOrderKindRepr::FileName,
			Self::ModificationDate => SortOrderKindRepr::ModificationDate,
			Self::Size => SortOrderKindRepr::Size,
			Self::Resolution(SortOrderResolutionDir::Width) => SortOrderKindRepr::ResolutionWidth,
			Self::Resolution(SortOrderResolutionDir::Height) => SortOrderKindRepr::ResolutionHeight,
		};

		repr.serialize(serializer)
	}
}

impl<'de> serde::Deserialize<'de> for SortOrderKind {
	fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
	where
		D: serde::Deserializer<'de>,
	{
		let repr = SortOrderKindRepr::deserialize(deserializer)?;
		let kind = match repr {
			SortOrderKindRepr::FileName => Self::FileName,
			SortOrderKindRepr::ModificationDate => Self::ModificationDate,
			SortOrderKindRepr::Size => Self::Size,
			SortOrderKindRepr::ResolutionWidth => Self::Resolution(SortOrderResolutionDir::Width),
			SortOrderKindRepr::ResolutionHeight => Self::Resolution(SortOrderResolutionDir::Height),
		};

		Ok(kind)
	}
}
