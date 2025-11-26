//! Utilities

// Imports
use core::cmp;

/// Error type for the crate.
pub type AppError = app_error::AppError<()>;

/// Extension trait to compare an option with a custom comparator
#[extend::ext(name = OptionCmpBy)]
pub impl<T> Option<T> {
	fn cmp_by(&self, other: &Self, f: impl FnOnce(&T, &T) -> cmp::Ordering) -> cmp::Ordering {
		match (self, other) {
			(None, None) => cmp::Ordering::Equal,
			(None, Some(_)) => cmp::Ordering::Less,
			(Some(_), None) => cmp::Ordering::Greater,
			(Some(lhs), Some(rhs)) => f(lhs, rhs),
		}
	}
}
