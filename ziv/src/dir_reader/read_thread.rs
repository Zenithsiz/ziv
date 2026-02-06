//! Read thread

// Imports
use {
	super::{
		CurEntry,
		DirEntry,
		entry::{EntryMetadata, EntrySource, EntrySourceZip},
	},
	crate::{dir_reader::entry::EntryFileType, util::AppError},
	app_error::Context,
	core::time::Duration,
	parking_lot::Mutex,
	std::{
		fs,
		path::Path,
		sync::{Arc, mpsc},
		time::SystemTime,
	},
	zip::ZipArchive,
	zutil_cloned::cloned,
};

/// Read thread
pub struct ReadThread {
	inner: Arc<Mutex<super::Inner>>,

	watch_event_tx: mpsc::Sender<notify_debouncer_full::DebounceEventResult>,
	watch_event_rx: mpsc::Receiver<notify_debouncer_full::DebounceEventResult>,
}

impl ReadThread {
	pub fn new(inner: Arc<Mutex<super::Inner>>) -> Self {
		let (watch_event_tx, watch_event_rx) = mpsc::channel();
		Self {
			inner,
			watch_event_tx,
			watch_event_rx,
		}
	}

	/// Reads a path into this directory reader.
	///
	/// Directories will be read, while files will have their parent read
	///
	/// # Errors
	/// If unable to read the directory or any of the entries, returns `Err`.
	///
	/// If processing the entries results in an error, logs it and continues
	// TODO: This should not be fallible
	// TODO: Instead of receiving a path, we should have a receiver of paths
	//       to read from.
	pub fn run(self, path: &Path) -> Result<(), AppError> {
		let path_metadata = path.metadata().context("Unable to get path metadata")?;

		match path_metadata.is_dir() {
			// If it's a directory, read it
			true => self.read_dir(path, None)?,
			// Otherwise, read it and the parent directory
			false => match self::try_archive(path) {
				Some(zip) => self.read_archive(path, zip)?,
				None => self.read_file_and_parent(path, &path_metadata)?,
			},
		}

		self.handle_watch_events();
	}

	/// Reads an archive as a directory
	fn read_archive(&self, path: &Path, zip: ZipArchive<fs::File>) -> Result<(), AppError> {
		let path = Arc::<Path>::from(path);

		let files_len = zip.len();
		let zip = Arc::new(Mutex::new(zip));
		for idx in 0..files_len {
			let (file_name, metadata, file_type) = {
				let mut zip = zip.lock();
				// TODO: `by_index_raw` also seeks the inner reader,
				//       which theoretically shouldn't perform any
				//       IO on `fs::File`, but can we avoid that?
				let file = zip.by_index_raw(idx).context("Unable to get file")?;

				let file_name = file.enclosed_name().context("File name wasn't enclosed")?;
				let file_type = match &file {
					_ if file.is_dir() => EntryFileType::Directory,
					_ if file.is_symlink() => EntryFileType::Symlink,
					_ => EntryFileType::File,
				};

				let modified_time_secs_from_epoch = file.extra_data_fields().find_map(|field| match field {
					zip::ExtraField::ExtendedTimestamp(timestamp) => timestamp.mod_time(),
					zip::ExtraField::Ntfs(_) => None,
				});
				let modified_time = match modified_time_secs_from_epoch {
					Some(secs_from_epoch) => SystemTime::UNIX_EPOCH + Duration::from_secs(secs_from_epoch.into()),
					// TODO: Should this be fallible?
					None => {
						let time = file.last_modified().context("File had no modified time")?;
						let time = time::OffsetDateTime::try_from(time).context("File had an invalid modified time")?;

						let unix_epoch = time::OffsetDateTime::UNIX_EPOCH;
						let time_from_epoch_abs = (time - unix_epoch).unsigned_abs();
						match time > unix_epoch {
							true => SystemTime::UNIX_EPOCH + time_from_epoch_abs,
							false => SystemTime::UNIX_EPOCH - time_from_epoch_abs,
						}
					},
				};

				// TODO: Should this be lazy-loaded?
				let metadata = EntryMetadata {
					modified_time,
					size: file.size(),
				};

				(file_name, metadata, file_type)
			};

			let zip = EntrySourceZip::new(Arc::clone(&path), file_name, metadata, idx, Arc::clone(&zip));
			let entry = DirEntry::new(EntrySource::Zip(Arc::new(zip)), file_type);
			self.inner.lock().insert(&entry)?;
		}

		Ok(())
	}

	/// Reads a file and it's parent directory
	fn read_file_and_parent(&self, path: &Path, file_metadata: &fs::Metadata) -> Result<(), AppError> {
		// Read the specified entry first, add it and set it as the current entry.
		let file_type = EntryFileType::from_file(file_metadata.file_type());
		let entry = self
			.read_path(path, Some(file_metadata), file_type)
			.context("Unable to read path")?;
		{
			let mut inner = self.inner.lock();
			inner.entries.insert(entry.clone());
			inner.cur_entry = Some(CurEntry { entry, idx: None });
		}

		// Then read the parent
		// Note: If `path` is just a filename, then it's parent will be empty,
		//       but walkdir doesn't like empty paths, so we replace them with `.`.
		let mut parent = path.parent().context("Path had no parent")?;
		if parent.is_empty() {
			parent = Path::new(".");
		}
		self.read_dir(parent, Some(path))
	}

	/// Reads a directory.
	///
	/// If `existing_entry` is `Some` and we find an entry with
	/// the same path, it will be ignored.
	fn read_dir(&self, path: &Path, existing_entry: Option<&Path>) -> Result<(), AppError> {
		// Before reading it, start a watcher
		// Note: We do it *before* reading to ensure we don't miss any new files
		//       added during the traversal later on.
		self.start_watcher(path)?;

		let scan_dir = walkdir::WalkDir::new(path).min_depth(1).max_depth(1);
		for entry in scan_dir {
			let entry = entry.context("Unable to read entry")?;
			let entry_path = entry.path();

			// Skip if we're already added this one
			if let Some(existing_entry) = existing_entry &&
				existing_entry == entry_path
			{
				continue;
			}

			// TODO: On windows, we get the metadata with the directory for free,
			//       so we should pass it along to `read_path` to set it on the
			//       entry, avoiding a re-reading it later on.
			let file_type = EntryFileType::from_file(entry.file_type());
			if let Err(err) = self.read_path(entry_path, None, file_type) {
				tracing::warn!("Unable to read directory entry {entry_path:?}: {err:?}");
			}
		}

		Ok(())
	}

	/// Starts the watcher
	fn start_watcher(&self, path: &Path) -> Result<(), AppError> {
		#[cloned(watch_event_tx = self.watch_event_tx)]
		let mut debouncer = notify_debouncer_full::new_debouncer(
			Duration::from_secs(1),
			None,
			move |res: notify_debouncer_full::DebounceEventResult| _ = watch_event_tx.send(res),
		)
		.context("Unable to create watch debouncer")?;
		debouncer
			.watch(path, notify::RecursiveMode::NonRecursive)
			.context("Unable to watch directory")?;

		Ok(())
	}

	/// Handles all watch events
	// TODO: Once we implement re-reading directories, this needs to not block
	// Note: For now this is necessary to ensure we don't drop the directory watcher,
	//       which would stop watching
	fn handle_watch_events(&self) -> ! {
		while let Ok(res) = self.watch_event_rx.recv() {
			match res {
				Ok(events) =>
					for event in events {
						self.handle_watch_event(event);
					},
				Err(errs) =>
					for err in errs {
						tracing::warn!("Received a filesystem error while watching: {:?}", AppError::new(&err));
					},
			}
		}

		unreachable!("Event sender was dropped");
	}

	/// Handles a watch event
	fn handle_watch_event(&self, event: notify_debouncer_full::DebouncedEvent) {
		tracing::trace!("Received watch event: {event:?}");
		match event.kind {
			notify::EventKind::Create(_) =>
				for path in event.event.paths {
					let res: Result<(), AppError> = try {
						let metadata =
							fs::metadata(&path).with_context(|| format!("Unable to get metadata of {path:?}"))?;
						let file_type = EntryFileType::from_file(metadata.file_type());
						self.read_path(&path, Some(&metadata), file_type)?;
					};

					if let Err(err) = res {
						tracing::warn!("Unable to read path {path:?}: {err:?}");
					}
				},

			notify::EventKind::Modify(notify::event::ModifyKind::Name(notify::event::RenameMode::Both)) => {
				let mut paths = event.event.paths.into_iter().array_chunks();
				for [from_path, to_path] in &mut paths {
					self.inner.lock().rename(&from_path, to_path);
				}

				let remaining = paths.into_remainder();
				if !remaining.is_empty() {
					tracing::warn!(
						"Ignoring remaining paths in rename event: {:?}",
						remaining.collect::<Vec<_>>()
					);
				}
			},

			// TODO: `Remove` events don't seem to be emitted, just `Modify(Name(From))`,
			//       so we also remove when that happens
			notify::EventKind::Remove(_) |
			notify::EventKind::Modify(notify::event::ModifyKind::Name(notify::event::RenameMode::From)) =>
				for path in event.event.paths {
					let res = self.inner.lock().remove_by_path(&path);
					if let Err(err) = res {
						tracing::warn!("Unable to remove entry {path:?}: {err:?}");
					}
				},
			_ => tracing::trace!("Ignoring watch event"),
		}
	}

	/// Reads a path
	fn read_path(
		&self,
		path: &Path,
		metadata: Option<&fs::Metadata>,
		file_type: EntryFileType,
	) -> Result<DirEntry, AppError> {
		// Create the entry and insert it
		let entry = DirEntry::new(EntrySource::Path(path.into()), file_type);
		if let Some(metadata) = metadata {
			let metadata = EntryMetadata::from_file(metadata).context("Unable to create entry metadata")?;
			entry.set_metadata(metadata);
		}

		self.inner.lock().insert(&entry)?;

		Ok(entry)
	}
}

/// Attempts to open a file as an archive
fn try_archive(path: &Path) -> Option<ZipArchive<fs::File>> {
	// TODO: Should we ignore *all* errors here?
	let file = fs::File::open(path).ok()?;
	let zip = ZipArchive::new(file).ok()?;

	Some(zip)
}
