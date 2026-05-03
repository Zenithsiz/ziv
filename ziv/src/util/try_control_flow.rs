//! Fallible control flow

use {
	super::AppError,
	core::{
		convert::Infallible,
		ops::{ControlFlow, FromResidual, Residual, Try},
	},
};

/// Control flow for `Result<T, E>`s.
#[derive(Clone, Copy, Debug)]
pub enum TryControlFlow<C, T, E> {
	Continue(C),
	BreakOk(T),
	BreakErr(E),
}

impl<C, T> TryControlFlow<C, T, AppError> {
	// Adds context to this error
	pub fn context(self, msg: &'static str) -> Self {
		match self {
			Self::BreakErr(err) => Self::BreakErr(err.context(msg)),
			_ => self,
		}
	}
}

impl<C, T, E> Try for TryControlFlow<C, T, E> {
	type Output = C;
	type Residual = TryControlFlow<!, T, E>;

	fn from_output(output: Self::Output) -> Self {
		Self::Continue(output)
	}

	fn branch(self) -> ControlFlow<Self::Residual, Self::Output> {
		match self {
			Self::Continue(value) => core::ops::ControlFlow::Continue(value),
			Self::BreakOk(value) => core::ops::ControlFlow::Break(TryControlFlow::BreakOk(value)),
			Self::BreakErr(err) => core::ops::ControlFlow::Break(TryControlFlow::BreakErr(err)),
		}
	}
}

impl<C, T, E> Residual<C> for TryControlFlow<!, T, E> {
	type TryType = TryControlFlow<C, T, E>;
}

impl<C, T, E> FromResidual<TryControlFlow<!, T, E>> for TryControlFlow<C, T, E> {
	fn from_residual(residual: TryControlFlow<!, T, E>) -> Self {
		match residual {
			TryControlFlow::Continue(never) => never,
			TryControlFlow::BreakOk(value) => Self::BreakOk(value),
			TryControlFlow::BreakErr(err) => Self::BreakErr(err),
		}
	}
}

impl<T, E> FromResidual<TryControlFlow<!, T, E>> for Result<T, E> {
	fn from_residual(residual: TryControlFlow<!, T, E>) -> Self {
		match residual {
			TryControlFlow::Continue(never) => never,
			TryControlFlow::BreakOk(value) => Ok(value),
			TryControlFlow::BreakErr(err) => Err(err),
		}
	}
}

impl<C, T, E> FromResidual<Result<Infallible, E>> for TryControlFlow<C, T, E> {
	fn from_residual(residual: Result<Infallible, E>) -> Self {
		match residual {
			Ok(never) => match never {},
			Err(err) => Self::BreakErr(err),
		}
	}
}
