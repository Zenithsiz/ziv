//! A loadable value

// Imports
use {super::AppError, app_error::app_error, std::thread};

/// Loadable value
// TODO: Move interior mutability to this?
#[derive(Debug)]
pub struct Loadable<T> {
	value: Option<T>,
	// TODO: Use a thread pool instead of a thread for each loadable.
	task:  Option<thread::JoinHandle<Result<T, AppError>>>,
}

impl<T> Loadable<T> {
	/// Creates a new, empty, loadable
	pub const fn new() -> Self {
		Self {
			value: None,
			task:  None,
		}
	}

	/// Sets the value, and removing the task loading it, if any
	pub fn set(&mut self, value: T) {
		self.value = Some(value);
		self.task = None;
	}

	/// Removes the value and the task loading it, if any
	pub fn remove(&mut self) {
		self.value = None;
		self.task = None;
	}

	/// Loads this value, blocking until loaded.
	pub fn load<F>(&mut self, load: F) -> Result<&T, AppError>
	where
		T: Send,
		F: FnOnce() -> Result<T, AppError> + Send,
	{
		// TODO: Use pattern matching once polonius comes around
		if self.value.is_some() {
			return Ok(self.value.as_ref().expect("Just checked"));
		}

		let value = match self.task.take() {
			Some(task) => task
				.join()
				.map_err(|err| app_error!("Loading thread panicked: {err:?}"))??,
			None => load()?,
		};

		let value = self.value.insert(value);
		Ok(value)
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
				let task = thread::spawn(load);
				self.task = Some(task);

				Ok(None)
			},
		}
	}
}
