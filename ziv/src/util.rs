//! Utilities

// Imports
use {
	app_error::Context,
	core::cmp,
	serde::de::DeserializeOwned,
	std::{fs, path::Path},
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
	fn mul_vec2(&self, v: egui::Vec2) -> Self {
		(self.to_vec2() * v).to_pos2()
	}

	/// Divides this point by a vector
	fn div_vec2(&self, v: egui::Vec2) -> Self {
		(self.to_vec2() / v).to_pos2()
	}
}

#[extend::ext(name = RectUtils)]
pub impl egui::Rect {
	/// Multiplies both the min and maximum by a scale
	fn mul_vec2(&self, v: egui::Vec2) -> Self {
		egui::Rect {
			min: self.min.mul_vec2(v),
			max: self.max.mul_vec2(v),
		}
	}

	/// Divides both the min and maximum by a scale
	fn div_vec2(&self, v: egui::Vec2) -> Self {
		egui::Rect {
			min: self.min.div_vec2(v),
			max: self.max.div_vec2(v),
		}
	}

	/// Scales this rectangle from it's minimum position
	fn scale_from_min2(&self, scale: egui::Vec2) -> Self {
		self.translate(-self.min.to_vec2())
			.mul_vec2(scale)
			.translate(self.min.to_vec2())
	}
}

/// Hot reloads a numeric value from a file
#[expect(dead_code, reason = "It should only be used for debugging")]
pub fn hot_reload<T: DeserializeOwned>(path: impl AsRef<Path>, default: T) -> T {
	let path = path.as_ref();
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
