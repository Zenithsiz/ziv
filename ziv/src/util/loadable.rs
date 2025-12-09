//! A loadable value

// Imports
use super::{AppError, PriorityThreadPool, priority_thread_pool::Priority};

/// Loadable value
#[derive(Debug)]
pub struct Loadable<T, E = AppError> {
	value:   Option<Result<T, E>>,
	task_rx: Option<oneshot::Receiver<Result<T, E>>>,
}

impl<T, E> Loadable<T, E> {
	/// Creates a new, empty, loadable
	pub const fn new() -> Self {
		Self {
			value:   None,
			task_rx: None,
		}
	}

	/// Sets the value, and removing the task loading it, if any
	pub fn set(&mut self, value: T) {
		self.value = Some(Ok(value));
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
	pub fn try_get(&mut self) -> Result<Option<&mut T>, &mut E> {
		// TODO: Use pattern matching once polonius comes around
		if self.value.is_some() {
			return self.value.as_mut().expect("Just checked").as_mut().map(Some);
		}

		let res = match self.task_rx.take() {
			Some(rx) => rx.recv().expect("Loading thread panicked"),
			None => return Ok(None),
		};

		let res = self.value.insert(res);
		res.as_mut().map(Some)
	}

	/// Loads this value, blocking until loaded.
	pub fn load<F>(&mut self, load: F) -> Result<&mut T, &mut E>
	where
		T: Send,
		F: FnOnce() -> Result<T, E> + Send,
	{
		// TODO: Use pattern matching once polonius comes around
		if self.value.is_some() {
			return self.value.as_mut().expect("Just checked").as_mut();
		}

		let res = match self.task_rx.take() {
			Some(rx) => rx.recv().expect("Loading thread panicked"),
			None => load(),
		};

		let res = self.value.insert(res);
		res.as_mut()
	}

	/// Tries to load the value
	pub fn try_load<F>(
		&mut self,
		thread_pool: &PriorityThreadPool,
		priority: Priority,
		load: F,
	) -> Result<Option<&mut T>, &mut E>
	where
		T: Send + 'static,
		E: Send + 'static,
		F: FnOnce() -> Result<T, E> + Send + 'static,
	{
		// TODO: Use pattern matching once polonius comes around
		if self.value.is_some() {
			return self.value.as_mut().expect("Just checked").as_mut().map(Some);
		}

		match &self.task_rx {
			Some(task_rx) => match task_rx.try_recv() {
				Ok(res) => {
					self.task_rx = None;
					self.value.insert(res).as_mut().map(Some)
				},
				Err(oneshot::TryRecvError::Empty) => Ok(None),
				Err(oneshot::TryRecvError::Disconnected) => panic!("Loading thread panicked"),
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
