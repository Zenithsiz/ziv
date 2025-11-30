//! Priority thread pool

use {
	super::AppError,
	app_error::Context,
	core::mem,
	parking_lot::{Condvar, Mutex},
	std::{collections::BinaryHeap, sync::Arc, thread},
	zutil_cloned::cloned,
};

/// Priority thread pool
#[derive(Clone, Debug)]
pub struct PriorityThreadPool {
	/// Inner
	inner: Arc<Inner>,
}

impl PriorityThreadPool {
	/// Creates a new thread pool
	pub fn new() -> Result<Self, AppError> {
		let inner = Arc::new(Inner {
			state:   Mutex::new(StateMut {
				tasks:    BinaryHeap::new(),
				finished: false,
				threads:  vec![],
			}),
			condvar: Condvar::new(),
		});
		let thread_count = match thread::available_parallelism() {
			Ok(count) => count.into(),
			Err(err) => {
				tracing::warn!("Unable to get available parallelism, assuming maximum of 1 thread: {err:?}");
				1
			},
		};

		for thread_idx in 0..thread_count {
			#[cloned(inner)]
			let thread = thread::Builder::new()
				.name(format!("worker${thread_idx}"))
				.spawn(move || Self::worker(&inner))
				.context("Unable to spawn thread")?;

			inner.state.lock().threads.push(thread);
		}

		Ok(Self { inner })
	}

	/// Spawns a function on this thread pool
	pub fn spawn<F: FnOnce() + Send + 'static>(&self, priority: Priority, run: F) {
		let task = Task {
			run: Box::new(run),
			priority,
		};

		self.inner.state.lock().tasks.push(task);
		self.inner.condvar.notify_one();
	}

	/// Worker function to run on each thread
	fn worker(inner: &Inner) {
		loop {
			// Get the next task
			let task = loop {
				// If we're finished, just exit early
				let mut state = inner.state.lock();
				if state.finished {
					// TODO: Should we finish the remaining items we have?
					//       Maybe we should by default, but offer the user
					//       a way to abort a task to avoid it from being
					//       waited on on exit
					return;
				}

				// Else check if we have a task and go to sleep if we don't have any
				match state.tasks.pop() {
					Some(task) => break task,
					None => inner.condvar.wait(&mut state),
				}
			};

			tracing::trace!(?task, "Executing task");
			(task.run)();
		}
	}
}

impl Drop for PriorityThreadPool {
	fn drop(&mut self) {
		// Set as finished and take the threads so we can join them
		let mut state = self.inner.state.lock();
		state.finished = true;
		let threads = mem::take(&mut state.threads);
		drop(state);

		// Then notify all workers to quit and wait for them.
		// TODO: Should we skip waiting for them?
		self.inner.condvar.notify_all();
		for thread in threads {
			if let Err(err) = thread.join() {
				tracing::error!("Thread panicked: {err:?}");
			}
		}
	}
}

#[derive(Debug)]
struct StateMut {
	/// Tasks
	tasks: BinaryHeap<Task>,

	/// If finished
	finished: bool,

	/// All threads
	threads: Vec<thread::JoinHandle<()>>,
}

/// Inner state
#[derive(Debug)]
struct Inner {
	/// Mutable state
	state: Mutex<StateMut>,

	/// Condition variable
	condvar: Condvar,
}

/// Task
#[derive(derive_more::Debug)]
pub struct Task {
	// TODO: Add a name for debugging?
	#[debug(ignore)]
	run:      Box<dyn FnOnce() + Send>,
	priority: Priority,
}

impl PartialEq for Task {
	fn eq(&self, other: &Self) -> bool {
		self.cmp(other).is_eq()
	}
}

impl Eq for Task {}

impl PartialOrd for Task {
	fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
		Some(self.cmp(other))
	}
}

impl Ord for Task {
	fn cmp(&self, other: &Self) -> std::cmp::Ordering {
		self.priority.cmp(&other.priority)
	}
}

/// Priority
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash, Debug)]
pub struct Priority(i64);

impl Priority {
	pub const DEFAULT: Self = Self(0);
	pub const HIGH: Self = Self(i64::MAX);
	pub const LOW: Self = Self(i64::MIN);
}
