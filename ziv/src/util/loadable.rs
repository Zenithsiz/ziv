//! A loadable value

// Imports
use {super::AppError, app_error::app_error};

/// Loadable value
pub struct Loadable<T> {
	value: Option<T>,
	// TODO: Use a thread pool instead of a thread for each loadable.
	task:  Option<std::thread::JoinHandle<Result<T, AppError>>>,
}

impl<T> Loadable<T> {
	/// Creates a new, empty, loadable
	pub const fn new() -> Self {
		Self {
			value: None,
			task:  None,
		}
	}

	/// Removes the value and the task loading it, if any
	pub fn remove(&mut self) {
		if let Some(_handle) = self.task.take() {
			// TODO: If we switch to async, abort the handle here
		}
		self.value = None;
	}

	/// Tries to load the value
	pub fn try_load<F>(&mut self, load: F) -> Result<Option<&T>, AppError>
	where
		T: Send + 'static,
		F: FnOnce() -> Result<T, AppError> + Send + 'static,
	{
		// TODO: Use pattern matching once polonius comes around
		if self.value.is_some() {
			return Ok(Some(self.value.as_ref().expect("Just checked")));
		}

		match self.task.take() {
			Some(task) => match task.is_finished() {
				true => {
					// Note: We return the inner error without any context
					let value = task
						.join()
						.map_err(|err| app_error!("Loading thread panicked: {err:?}"))??;
					let value = self.value.insert(value);

					Ok(Some(value))
				},
				false => {
					self.task = Some(task);
					Ok(None)
				},
			},
			None => {
				let task = std::thread::spawn(load);
				self.task = Some(task);

				Ok(None)
			},
		}
	}
}
