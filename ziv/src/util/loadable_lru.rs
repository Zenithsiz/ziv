//! Loadable lru

// Imports
use {
	super::AppError,
	crate::util::{PriorityThreadPool, priority_thread_pool::Priority},
	core::hash::Hash,
	hashlink::LruCache,
	parking_lot::{Mutex, MutexGuard},
	std::sync::Arc,
	zutil_cloned::cloned,
};

#[derive(Debug)]
enum State<V, E> {
	Loading,
	Loaded(Result<V, E>),
}

/// Loadable map
///
/// A lru of loadable values
#[derive(Debug)]
pub struct LoadableLru<K, V, E = AppError> {
	values: Arc<Mutex<LruCache<K, State<V, E>>>>,
}

impl<K, V, E> LoadableLru<K, V, E>
where
	K: Eq + Hash,
{
	/// Creates an lru with `capacity` capacity
	pub fn new(capacity: usize) -> Self {
		Self {
			values: Arc::new(Mutex::new(LruCache::new(capacity))),
		}
	}

	/// Sets the maximum capacity for the lru
	pub fn set_max(&self, max: usize) {
		let mut values = self.values.lock();
		values.set_capacity(max);
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
		let mut values = self.values.lock();
		match values.get(key) {
			Some(state) => match state {
				State::Loading => Ok(None),
				State::Loaded(res) => res.clone().map(Some),
			},
			None => {
				let loader = loader.into_loader();

				#[cloned(values = self.values, key)]
				thread_pool.spawn(priority, move || {
					// If we aren't in the values anymore, then we're no longer relevant
					// and we can skip loading
					let mut values = values.lock();
					if !values.contains_key(&key) {
						return;
					}

					// Note: Ensure we don't keep `values` locked while loading
					let res = MutexGuard::unlocked(&mut values, || loader.load(&key));

					// Finally, update ourselves as loaded
					values.insert(key, State::Loaded(res));
				});
				values.insert(key.clone(), State::Loading);

				Ok(None)
			},
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
