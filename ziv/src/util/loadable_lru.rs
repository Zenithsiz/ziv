//! Loadable lru

// Imports
use {
	super::AppError,
	crate::util::{PriorityThreadPool, priority_thread_pool::Priority},
	core::hash::Hash,
	hashlink::LruCache,
	parking_lot::{Condvar, Mutex, MutexGuard},
	std::sync::Arc,
	zutil_cloned::cloned,
};

#[derive(Debug)]
enum State<V, E> {
	Loading,
	Loaded(Result<V, E>),
}

#[derive(Debug)]
struct Inner<K, V, E> {
	values:  Mutex<LruCache<K, State<V, E>>>,
	condvar: Condvar,
}

/// Loadable map
///
/// A lru of loadable values
#[derive(Debug, Clone)]
pub struct LoadableLru<K, V, E = AppError> {
	inner: Arc<Inner<K, V, E>>,
}

impl<K, V, E> LoadableLru<K, V, E>
where
	K: Eq + Hash,
{
	/// Creates an lru with `capacity` capacity
	pub fn new(capacity: usize) -> Self {
		Self {
			inner: Arc::new(Inner {
				values:  Mutex::new(LruCache::new(capacity)),
				condvar: Condvar::new(),
			}),
		}
	}

	/// Sets the maximum capacity for the lru
	pub fn set_max(&self, max: usize) {
		let mut values = self.inner.values.lock();
		values.set_capacity(max);
	}

	/// Gets a value by it's key or loads it by blocking
	pub fn get_or_load_blocking<L>(&self, key: &K, loader: L) -> Result<V, E>
	where
		K: Clone,
		V: Clone,
		E: Clone,
		L: Loader<K, V, E>,
	{
		let mut values = self.inner.values.lock();
		loop {
			match values.get(key) {
				Some(state) => match state {
					State::Loading => self.inner.condvar.wait(&mut values),
					State::Loaded(value) => break value.clone(),
				},
				None => {
					let res = loader.load(key);
					values.insert(key.clone(), State::Loaded(res.clone()));
					self.inner.condvar.notify_all();
					break res;
				},
			}
		}
	}

	/// Gets a value by it's key or starts loading it
	pub fn get_or_load<L>(
		&self,
		key: &K,
		thread_pool: &PriorityThreadPool,
		priority: Priority,
		loader: L,
	) -> Result<Option<V>, E>
	where
		K: Clone + Send + 'static,
		V: Clone + Send + 'static,
		E: Clone + Send + 'static,
		L: IntoLoader<K, V, E>,
		L::Loader: Send + 'static,
	{
		let mut values = self.inner.values.lock();
		match values.get(key) {
			Some(state) => match state {
				State::Loading => Ok(None),
				State::Loaded(res) => res.clone().map(Some),
			},
			None => {
				let loader = loader.into_loader();

				#[cloned(inner = self.inner, key)]
				thread_pool.spawn(priority, move || {
					// If we aren't in the values anymore, then we're no longer relevant
					// and we can skip loading
					let mut values = inner.values.lock();
					if !values.contains_key(&key) {
						return;
					}

					// Note: Ensure we don't keep `values` locked while loading
					let res = MutexGuard::unlocked(&mut values, || loader.load(&key));

					// Finally, update ourselves as loaded
					values.insert(key, State::Loaded(res));

					inner.condvar.notify_all();
				});
				values.insert(key.clone(), State::Loading);

				Ok(None)
			},
		}
	}

	/// Gets a value by it's key, if loaded
	pub fn get(&self, key: &K) -> Result<Option<V>, E>
	where
		V: Clone,
		E: Clone,
	{
		let mut values = self.inner.values.lock();
		match values.get(key) {
			Some(state) => match state {
				State::Loading => Ok(None),
				State::Loaded(value) => value.clone().map(Some),
			},
			None => Ok(None),
		}
	}
}

pub trait IntoLoader<K, V, E> {
	type Loader: Loader<K, V, E>;
	fn into_loader(self) -> Self::Loader;
}

impl<K, V, E, F, L> IntoLoader<K, V, E> for F
where
	F: FnOnce() -> L,
	L: Loader<K, V, E>,
{
	type Loader = L;

	fn into_loader(self) -> Self::Loader {
		self()
	}
}


pub trait Loader<K, V, E> {
	fn load(self, key: &K) -> Result<V, E>;
}

impl<K, V, E, F> Loader<K, V, E> for F
where
	F: FnOnce(&K) -> Result<V, E>,
{
	fn load(self, key: &K) -> Result<V, E> {
		self(key)
	}
}
