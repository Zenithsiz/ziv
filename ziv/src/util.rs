//! Utilities

// Modules
pub mod config;
pub mod loadable;
pub mod priority_thread_pool;

// Exports
pub use self::{loadable::Loadable, priority_thread_pool::PriorityThreadPool};

// Imports
use {
	app_error::Context,
	core::{
		cmp,
		convert::Infallible,
		fmt::{self, Debug},
		ops::{FromResidual, Try},
		time::Duration,
	},
	serde::{Serialize, de::DeserializeOwned},
	std::{
		collections::{BTreeMap, HashMap},
		fs,
		io,
		ops::ControlFlow,
		path::Path,
		time::Instant,
	},
};

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

#[extend::ext(name = OptionGetOrTryInsert)]
pub impl<T> Option<T> {
	fn get_or_try_insert_with<E>(&mut self, f: impl FnOnce() -> Result<T, E>) -> Result<&mut T, E> {
		match self {
			Some(value) => Ok(value),
			None => Ok(self.insert(f()?)),
		}
	}
}

#[extend::ext(name = OptionInspectNone)]
pub impl<T> Option<T> {
	fn inspect_none(self, f: impl FnOnce()) -> Self {
		if self.is_none() {
			f();
		}

		self
	}
}


#[extend::ext(name = Pos2Utils)]
pub impl egui::Pos2 {
	/// Multiplies this point by a vector
	fn mul_vec2(self, v: egui::Vec2) -> Self {
		(self.to_vec2() * v).to_pos2()
	}

	/// Divides this point by a vector
	fn div_vec2(self, v: egui::Vec2) -> Self {
		(self.to_vec2() / v).to_pos2()
	}
}

#[extend::ext(name = RectUtils)]
pub impl egui::Rect {
	/// Multiplies both the min and maximum by a scale
	fn mul_vec2(self, v: egui::Vec2) -> Self {
		egui::Rect {
			min: self.min.mul_vec2(v),
			max: self.max.mul_vec2(v),
		}
	}

	/// Divides both the min and maximum by a scale
	fn div_vec2(self, v: egui::Vec2) -> Self {
		egui::Rect {
			min: self.min.div_vec2(v),
			max: self.max.div_vec2(v),
		}
	}

	/// Scales this rectangle from it's minimum position
	fn scale_from_min(self, scale: f32) -> Self {
		self.scale_from_center2(egui::Vec2::splat(scale))
	}

	/// Scales this rectangle from it's minimum position
	fn scale_from_min2(mut self, scale: egui::Vec2) -> Self {
		self.max = self.min + self.size() * scale;
		self
	}
}

#[extend::ext(name = InstantSaturatingOps)]
pub impl Instant {
	fn saturating_sub(&self, duration: Duration) -> Self {
		match self.checked_sub(duration) {
			Some(instant) => instant,
			None => *self,
		}
	}
}

/// Hot reloads a numeric value from a file
#[expect(dead_code, reason = "It should only be used for debugging")]
pub fn hot_reload<T: Serialize + DeserializeOwned>(path: impl AsRef<Path>, default: T) -> T {
	let path = path.as_ref();
	if fs::exists(path).is_ok_and(|exists| !exists) {
		let res: Result<_, AppError> = try {
			let contents = toml::to_string_pretty(&default)?;
			fs::write(path, &contents)?;
		};

		if let Err(err) = res {
			tracing::warn!("Unable to write default value to {path:?}: {err:?}");
		}
	}

	let res: Result<_, AppError> = try {
		let contents = fs::read(path).context("Unable to read file")?;
		toml::from_slice(&contents).context("Unable to parse file")?
	};

	match res {
		Ok(num) => num,
		Err(err) => {
			tracing::warn!("Unable to hot-reload value from file {path:?}: {err:?}");
			default
		},
	}
}

/// Creates the parent directory of a file, if it has a parent
pub fn create_parent(path: impl AsRef<Path>) -> Result<(), io::Error> {
	let path = path.as_ref();
	match path.parent() {
		Some(parent) => fs::create_dir_all(parent),
		None => Ok(()),
	}
}

/// Serializes a hashmap with the keys sorted
pub fn hashmap_serialize_sorted<K, V, S>(hashmap: &HashMap<K, V>, serializer: S) -> Result<S::Ok, S::Error>
where
	K: Ord + Serialize,
	V: Serialize,
	S: serde::Serializer,
{
	let map = hashmap.iter().collect::<BTreeMap<&K, &V>>();
	map.serialize(serializer)
}


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

/// Formats a duration, with a minimum of second precision
pub fn format_duration(duration: Duration) -> impl fmt::Debug {
	fmt::from_fn(move |f| {
		let mut secs = duration.as_secs_f64();

		let hours = (secs / 3600.0).floor();
		secs -= hours * 3600.0;

		let mins = (secs / 60.0).floor();
		secs -= mins * 60.0;

		if hours != 0.0 {
			write!(f, "{hours}h")?;
		}
		if mins != 0.0 {
			write!(f, "{mins}m")?;
		}
		secs.fmt(f)?;
		f.pad("s")?;

		Ok(())
	})
}

/// Clones the value in a `Result<&mut T, E>`
#[extend::ext(name = ResultClonedMut)]
pub impl<T, E> Result<&mut T, E> {
	fn cloned_mut(self) -> Result<T, E>
	where
		T: Clone,
	{
		self.map(|value| value.clone())
	}
}

/// Clones the value in an `Option<&mut T>`
#[extend::ext(name = OptionClonedMut)]
pub impl<T> Option<&mut T> {
	fn cloned_mut(self) -> Option<T>
	where
		T: Clone,
	{
		self.map(|value| value.clone())
	}
}

/// Clones the error in a `Result<T, &mut E>`
#[extend::ext(name = ResultErrClonedMut)]
pub impl<T, E> Result<T, &mut E> {
	fn cloned_err_mut(self) -> Result<T, E>
	where
		E: Clone,
	{
		self.map_err(|err| err.clone())
	}
}

/// Wrapper around `egui::TextureHandle`
#[derive(PartialEq, Eq, Clone, Hash, derive_more::Debug)]
#[derive(derive_more::Deref, derive_more::DerefMut)]
#[debug("{:?}", self.0.id())]
pub struct EguiTextureHandle(pub egui::TextureHandle);

impl From<&'_ EguiTextureHandle> for egui::load::SizedTexture {
	fn from(handle: &EguiTextureHandle) -> Self {
		Self::from_handle(&handle.0)
	}
}
