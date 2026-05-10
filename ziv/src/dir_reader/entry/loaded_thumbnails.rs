//! Entry thumbnails

// Imports
use {
	super::{DirEntry, EntryThumbnail, ThumbnailDb},
	crate::{
		dir_reader::DirReader,
		util::{AppError, LoadableLru, PriorityThreadPool, priority_thread_pool::Priority},
	},
	app_error::Context,
	zutil_cloned::cloned,
};


/// Entry loaded thumbnails
///
/// Manages all loaded thumbnails
#[derive(Debug)]
pub struct EntryLoadedThumbnails {
	thumbnails: LoadableLru<DirEntry, EntryThumbnail>,
}

impl EntryLoadedThumbnails {
	/// Creates the thumbnails, with none loaded
	pub fn new() -> Self {
		Self {
			// Note: At the start, we allow an unbounded number of thumbnails,
			//       since we'll only get resized after each frame, when the user
			//       knows how many thumbnails they rendered.
			thumbnails: LoadableLru::new(usize::MAX),
		}
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
		thumbnails_db: &ThumbnailDb,
	) -> Result<Option<EntryThumbnail>, AppError> {
		self.thumbnails.get_or_load(entry, thread_pool, Priority::LOW, move || {
			let mut thumbnail_progress = dir_reader.thumbnail_progress_update();
			thumbnail_progress.set_loading();

			#[cloned(egui_ctx, thumbnails_db)]
			move |entry: &DirEntry| {
				let source = entry.source();
				let data = entry.data();

				// TODO: Requesting a repaint inside of this closure is the wrong
				//       thing to do, since it's possible for that repaint to happen
				//       before the thumbnail gets added to the lru.
				EntryThumbnail::new(&egui_ctx, &thumbnails_db, &source, data, &mut thumbnail_progress)
					.inspect(|_| egui_ctx.request_repaint())
					.context("Unable to create thumbnail")
			}
		})
	}
}
