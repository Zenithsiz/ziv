//! Entry displays

// Imports
use {
	super::{DirEntry, EntryDisplay},
	crate::{
		dir_reader::entry::{EntryImage, EntrySource, EntryVideo},
		util::{AppError, LoadableLru, PriorityThreadPool, priority_thread_pool::Priority},
	},
	app_error::{Context, bail},
	image::ImageFormat,
	std::{
		ffi::OsStr,
		fs,
		io::{self, Read},
	},
};


/// Entry loaded displays
///
/// Manages all loaded displays
#[derive(Clone, Debug)]
pub struct EntryLoadedDisplays {
	displays: LoadableLru<DirEntry, EntryDisplay>,
}

impl EntryLoadedDisplays {
	/// Creates the displays, with none loaded
	pub fn new(capacity: usize) -> Self {
		Self {
			displays: LoadableLru::new(capacity),
		}
	}

	/// Sets the maximum number of displays
	pub fn _set_max(&self, max: usize) {
		self.displays.set_max(max);
	}

	/// Gets an entry's display, loading it in-place if unloaded
	pub fn get_blocking(&self, entry: &DirEntry) -> Result<EntryDisplay, AppError> {
		self.displays
			.get_or_load_blocking(entry, move |entry: &DirEntry| self::load(entry))
	}

	/// Gets an entry's display
	pub fn get(&self, entry: &DirEntry, thread_pool: &PriorityThreadPool) -> Result<Option<EntryDisplay>, AppError> {
		self.displays.get_or_load(entry, thread_pool, Priority::LOW, move || {
			move |entry: &DirEntry| self::load(entry)
		})
	}

	/// Gets an entry's display, if loaded
	pub fn get_if_loaded(&self, entry: &DirEntry) -> Result<Option<EntryDisplay>, AppError> {
		self.displays.get(entry)
	}
}


fn load(entry: &DirEntry) -> Result<EntryDisplay, AppError> {
	let source = entry.source();
	let file_name = entry.file_name().context("Entry had no file name")?;

	// If it's a directory it's non-media
	if entry.file_type().is_dir() {
		bail!("Directories have no display");
	}

	// Test against common zip archives
	const COMMON_ZIP_FORMATS: &[&str] = &["zip", "cbz"];
	if let Some(ext) = file_name.extension().and_then(OsStr::to_str) &&
		COMMON_ZIP_FORMATS.contains(&ext)
	{
		bail!("Archives have no display");
	}

	// Test against video file formats before we need to read the file.
	const COMMON_VIDEO_FORMATS: &[&str] = &["gif", "mkv", "mp4", "mov", "avi", "webm"];
	if let Some(ext) = file_name.extension().and_then(OsStr::to_str) &&
		COMMON_VIDEO_FORMATS.contains(&ext)
	{
		// TODO: Support videos inside of other sources
		let EntrySource::Path(path) = source else {
			app_error::bail!("Videos are only supported by path")
		};

		let video = EntryVideo::new(path).context("Unable to create video")?;
		return Ok(EntryDisplay::Video(video));
	}

	// If we got a format just from the path, return it
	// TODO: Should we trust the extension?
	if let Some(ext) = file_name.extension() &&
		let Some(format) = ImageFormat::from_extension(ext)
	{
		let image = EntryImage::new(source, format);
		return Ok(EntryDisplay::Image(image));
	}

	// If it's a directory it's non-media
	// Note: This check involves touching the filesystem, so we only
	//       do it after checking everything we can without IO
	let is_dir = match &source {
		EntrySource::Path(path) => fs::metadata(path).context("Unable to get file metadata")?.is_dir(),
		EntrySource::Zip(zip) => zip.is_dir().context("Unable to check if zip entry was a directory")?,
	};
	if is_dir {
		bail!("Directories have no display");
	}

	// Otherwise, try to guess it by opening it with `image`
	let format = match &source {
		EntrySource::Path(path) => ::image::ImageReader::open(path)
			.context("Unable to create image reader")?
			.with_guessed_format()
			.context("Unable to read file")?
			.format(),

		// TODO: `with_guessed_format` only uses the first 16 bytes, so we just read that
		EntrySource::Zip(zip) => zip
			.try_with_file(|mut file| {
				let mut contents = [0; 16];
				file.read_exact(&mut contents).context("Unable to read contents")?;
				let format = ::image::ImageReader::new(io::Cursor::new(contents))
					.with_guessed_format()
					.context("Unable to read file")?
					.format();

				Ok(format)
			})
			.context("Unable to guess zip entry format")?,
	};
	if let Some(format) = format {
		let image = EntryImage::new(source, format);
		return Ok(EntryDisplay::Image(image));
	}

	// Then finally, try to guess it with `ffmpeg`.
	// Note: This detects quite a few thing we don't want to open,
	//       such as text files, so we ignore part of the output.
	// TODO: Currently we detect `svg`s as a video format (using
	//       `svg_pipe`). However, `image` doesn't support `svg`s,
	//       so for now we allow this.
	// TODO: Support videos inside of other sources
	if let EntrySource::Path(path) = source {
		const DISALLOWED_VIDEO_FORMATS: &[&str] = &["lrc", "tty", "mjpeg"];
		if let Ok(input) = ffmpeg_next::format::input(&path) &&
			!DISALLOWED_VIDEO_FORMATS.contains(&input.format().name())
		{
			let video = EntryVideo::new(path).context("Unable to create video")?;
			return Ok(EntryDisplay::Video(video));
		}
	}

	bail!("Unable to display unknown file type");
}
