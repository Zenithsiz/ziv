//! Entry thumbnails

// Imports
use {
	super::{DirEntry, EntryThumbnail},
	crate::{
		dir_reader::{DirReader, ThumbnailProgressGuard},
		util::{AppError, PriorityThreadPool, priority_thread_pool::Priority},
	},
	app_error::Context,
	hashlink::LruCache,
	parking_lot::{Mutex, MutexGuard},
	std::{path::Path, sync::Arc},
	zutil_cloned::cloned,
};


#[derive(Debug)]
enum ThumbnailState {
	Loading,
	Loaded(Result<EntryThumbnail, AppError>),
}

/// Entry thumbnails
///
/// Manages all thumbnails for every entry
#[derive(Debug)]
pub struct EntryThumbnails {
	thumbnails: Arc<Mutex<LruCache<DirEntry, ThumbnailState>>>,

	// TODO: We shouldn't have to care about the specified/default
	//       and instead should just receive a single directory.
	specified_dir: Option<Arc<Path>>,
	default_dir:   Arc<Path>,
}

impl EntryThumbnails {
	/// Creates the thumbnails, with none loaded
	pub fn new(specified_dir: Option<Arc<Path>>, default_dir: Arc<Path>) -> Self {
		Self {
			// Note: At the start, we allow an unbounded number of thumbnails,
			//       since we'll only get resized after each frame, when the user
			//       knows how many thumbnails they rendered.
			thumbnails: Arc::new(Mutex::new(LruCache::new_unbounded())),
			specified_dir,
			default_dir,
		}
	}

	/// Returns the specified thumbnails directory, if any
	pub const fn specified_dir(&self) -> Option<&Arc<Path>> {
		self.specified_dir.as_ref()
	}

	/// Sets the maximum number of thumbnails
	pub fn set_max(&self, max: usize) {
		let mut thumbnails = self.thumbnails.lock();
		thumbnails.set_capacity(max);
	}

	/// Gets an entry's thumbnail
	pub fn get(
		&self,
		entry: &DirEntry,
		dir_reader: &DirReader,
		thread_pool: &PriorityThreadPool,
		egui_ctx: &egui::Context,
	) -> Result<Option<EntryThumbnail>, AppError> {
		let mut thumbnails = self.thumbnails.lock();
		match thumbnails.get(entry) {
			Some(state) => match state {
				ThumbnailState::Loading => Ok(None),
				ThumbnailState::Loaded(res) => res.clone().map(Some),
			},
			None => {
				let mut thumbnail_progress = dir_reader.thumbnail_progress_update();
				thumbnail_progress.set_loading();

				let thumbnails_dir = Arc::clone(self.specified_dir.as_ref().unwrap_or(&self.default_dir));

				#[cloned(thumbnails = self.thumbnails, entry, egui_ctx)]
				thread_pool.spawn(Priority::LOW, move || {
					// If we aren't in the thumbnails anymore, then we're no longer relevant
					// and we can skip loading
					let mut thumbnails = thumbnails.lock();
					if !thumbnails.contains_key(&entry) {
						return;
					}

					// Note: Ensure we don't keep `shared` locked while loading
					let res = MutexGuard::unlocked(&mut thumbnails, || {
						self::load(&entry, &thumbnails_dir, &egui_ctx, &mut thumbnail_progress)
					});

					// Finally, update ourselves as loaded
					thumbnails.insert(entry, ThumbnailState::Loaded(res));
					egui_ctx.request_repaint();
				});
				thumbnails.insert(entry.clone(), ThumbnailState::Loading);

				Ok(None)
			},
		}
	}
}

fn load(
	entry: &DirEntry,
	thumbnails_dir: &Path,
	egui_ctx: &egui::Context,
	thumbnail_progress: &mut ThumbnailProgressGuard,
) -> Result<EntryThumbnail, app_error::AppError> {
	let source = entry.source();
	let data = entry.data_blocking().context("Unable to load data")?;
	EntryThumbnail::new(egui_ctx, thumbnails_dir, &source, &data, thumbnail_progress)
		.context("Unable to create thumbnail")
}
