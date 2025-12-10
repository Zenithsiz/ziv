//! A loadable value

// Imports
use {
	super::{AppError, PriorityThreadPool, priority_thread_pool::Priority},
	parking_lot::{Mutex, MutexGuard},
};

#[derive(Debug)]
struct Inner<T, E> {
	value:   Option<Result<T, E>>,
	task_rx: Option<oneshot::Receiver<Result<T, E>>>,
}

/// Loadable value
#[derive(Debug)]
pub struct Loadable<T, E = AppError> {
	inner: Mutex<Inner<T, E>>,
}

impl<T, E> Loadable<T, E> {
	/// Creates a new, empty, loadable
	pub const fn new() -> Self {
		Self {
			inner: Mutex::new(Inner {
				value:   None,
				task_rx: None,
			}),
		}
	}

	/// Creates a new, loaded, loadable
	pub const fn loaded(value: T) -> Self {
		Self {
			inner: Mutex::new(Inner {
				value:   Some(Ok(value)),
				task_rx: None,
			}),
		}
	}

	/// Returns if this loadable has a value.
	///
	/// Does not check if the loading task exists or is done.
	pub fn is_loaded(&self) -> bool {
		self.inner.lock().value.is_some()
	}

	/// Sets the value, and removing the task loading it, if any
	pub fn set(&self, value: T) {
		let mut inner = self.inner.lock();
		inner.value = Some(Ok(value));
		inner.task_rx = None;
	}

	/// Removes the value and the task loading it, if any
	pub fn remove(&self) {
		let mut inner = self.inner.lock();
		inner.value = None;
		inner.task_rx = None;
	}

	/// Tries to get the value.
	///
	/// If unloaded and no value ready from the task, returns `Ok(None)`
	pub fn try_get(&self) -> Result<Option<T>, E>
	where
		T: Clone,
		E: Clone,
	{
		// TODO: Use pattern matching once polonius comes around
		let mut inner = self.inner.lock();
		if inner.value.is_some() {
			return inner.value.as_ref().expect("Just checked").clone().map(Some);
		}

		let res = match &mut inner.task_rx {
			Some(rx) => match rx.try_recv() {
				Ok(res) => {
					inner.task_rx = None;
					res
				},
				Err(oneshot::TryRecvError::Empty) => return Ok(None),
				Err(oneshot::TryRecvError::Disconnected) => panic!("Loading thread panicked"),
			},
			None => return Ok(None),
		};

		inner.value = Some(res.clone());
		drop(inner);

		res.map(Some)
	}

	/// Loads this value, blocking until loaded.
	pub fn load<F>(&self, load: F) -> Result<T, E>
	where
		T: Clone,
		E: Clone,
		F: FnOnce() -> Result<T, E>,
	{
		// TODO: Use pattern matching once polonius comes around
		let mut inner = self.inner.lock();
		if inner.value.is_some() {
			return inner.value.as_mut().expect("Just checked").clone();
		}

		let task_rx = inner.task_rx.take();
		let res = MutexGuard::unlocked(&mut inner, || match task_rx {
			Some(rx) => rx.recv().expect("Loading thread panicked"),
			None => load(),
		});

		inner.value = Some(res.clone());
		res
	}

	/// Tries to load the value
	pub fn try_load<F>(&self, thread_pool: &PriorityThreadPool, priority: Priority, load: F) -> Result<Option<T>, E>
	where
		T: Send + Clone + 'static,
		E: Send + Clone + 'static,
		F: FnOnce() -> Result<T, E> + Send + 'static,
	{
		// TODO: Use pattern matching once polonius comes around
		let mut inner = self.inner.lock();
		if inner.value.is_some() {
			return inner.value.as_mut().expect("Just checked").clone().map(Some);
		}

		match &inner.task_rx {
			Some(task_rx) => match task_rx.try_recv() {
				Ok(res) => {
					inner.task_rx = None;
					inner.value = Some(res.clone());

					res.map(Some)
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
				inner.task_rx = Some(task_rx);
				drop(inner);

				Ok(None)
			},
		}
	}
}
