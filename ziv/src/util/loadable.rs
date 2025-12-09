//! A loadable value

// Imports
use {
	super::{AppError, PriorityThreadPool, priority_thread_pool::Priority},
	app_error::app_error,
};

/// Loadable value
// TODO: Move interior mutability to this?
#[derive(Debug)]
pub struct Loadable<T> {
	value:   Option<T>,
	task_rx: Option<oneshot::Receiver<Result<T, AppError>>>,
}

impl<T> Loadable<T> {
	/// Creates a new, empty, loadable
	pub const fn new() -> Self {
		Self {
			value:   None,
			task_rx: None,
		}
	}

	/// Sets the value, and removing the task loading it, if any
	pub fn set(&mut self, value: T) {
		self.value = Some(value);
		self.task_rx = None;
	}

	/// Removes the value and the task loading it, if any
	pub fn remove(&mut self) {
		self.value = None;
		self.task_rx = None;
	}

	/// Tries to get the value.
	///
	/// If unloaded and no value ready from the task, returns `Ok(None)`
	pub fn try_get(&mut self) -> Result<Option<&T>, AppError> {
		// TODO: Use pattern matching once polonius comes around
		if self.value.is_some() {
			return Ok(Some(self.value.as_ref().expect("Just checked")));
		}

		let value = match self.task_rx.take() {
			Some(rx) => rx.recv().map_err(|_| app_error!("Loading thread closed"))??,
			None => return Ok(None),
		};

		let value = self.value.insert(value);
		Ok(Some(value))
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

		let value = match self.task_rx.take() {
			Some(rx) => rx.recv().map_err(|_| app_error!("Loading thread closed"))??,
			None => load()?,
		};

		let value = self.value.insert(value);
		Ok(value)
	}

	/// Tries to load the value
	pub fn try_load<F>(
		&mut self,
		thread_pool: &PriorityThreadPool,
		priority: Priority,
		load: F,
	) -> Result<Option<&T>, AppError>
	where
		T: Send + 'static,
		F: FnOnce() -> Result<T, AppError> + Send + 'static,
	{
		// TODO: Use pattern matching once polonius comes around
		if self.value.is_some() {
			return Ok(Some(self.value.as_ref().expect("Just checked")));
		}

		match &self.task_rx {
			Some(task_rx) => match task_rx.try_recv() {
				Ok(res) => {
					self.task_rx = None;
					let value = res?;
					Ok(Some(self.value.insert(value)))
				},
				Err(oneshot::TryRecvError::Empty) => Ok(None),
				Err(oneshot::TryRecvError::Disconnected) => app_error::bail!("Loading thread closed"),
			},
			None => {
				let (task_tx, task_rx) = oneshot::channel();
				thread_pool.spawn(priority, move || {
					let res = load();
					_ = task_tx.send(res);
				});
				self.task_rx = Some(task_rx);

				Ok(None)
			},
		}
	}
}
