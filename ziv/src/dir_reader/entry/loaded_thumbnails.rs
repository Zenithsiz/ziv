//! Entry thumbnails

// Imports
use {
	super::{DirEntry, EntryThumbnail},
	crate::{
		dir_reader::DirReader,
		util::{AppError, LoadableLru, PriorityThreadPool, priority_thread_pool::Priority},
	},
	app_error::Context,
	std::{path::Path, sync::Arc},
	zutil_cloned::cloned,
};


/// Entry loaded thumbnails
///
/// Manages all loaded thumbnails
#[derive(Debug)]
pub struct EntryLoadedThumbnails {
	thumbnails: LoadableLru<DirEntry, EntryThumbnail>,

	// TODO: We shouldn't have to care about the specified/default
	//       and instead should just receive a single directory.
	specified_dir: Option<Arc<Path>>,
	default_dir:   Arc<Path>,
}

impl EntryLoadedThumbnails {
	/// Creates the thumbnails, with none loaded
	pub fn new(specified_dir: Option<Arc<Path>>, default_dir: Arc<Path>) -> Self {
		Self {
			// Note: At the start, we allow an unbounded number of thumbnails,
			//       since we'll only get resized after each frame, when the user
			//       knows how many thumbnails they rendered.
			thumbnails: LoadableLru::new(usize::MAX),
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
		self.thumbnails.set_max(max);
	}

	/// Gets an entry's thumbnail
	pub fn get(
		&self,
		entry: &DirEntry,
		dir_reader: &DirReader,
		thread_pool: &PriorityThreadPool,
		egui_ctx: &egui::Context,
	) -> Result<Option<EntryThumbnail>, AppError> {
		let thumbnails_dir = self.specified_dir.as_ref().unwrap_or(&self.default_dir);
		self.thumbnails.get_or_load(entry, thread_pool, Priority::LOW, move || {
			let mut thumbnail_progress = dir_reader.thumbnail_progress_update();
			thumbnail_progress.set_loading();

			#[cloned(egui_ctx, thumbnails_dir)]
			move |entry: &DirEntry| {
				let source = entry.source();
				let data = entry.data();

				// TODO: Requesting a repaint inside of this closure is the wrong
				//       thing to do, since it's possible for that repaint to happen
				//       before the thumbnail gets added to the lru.
				EntryThumbnail::new(&egui_ctx, &thumbnails_dir, &source, data, &mut thumbnail_progress)
					.inspect(|_| egui_ctx.request_repaint())
					.context("Unable to create thumbnail")
			}
		})
	}
}
