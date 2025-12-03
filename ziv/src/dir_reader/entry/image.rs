//! Entry image

use {
	crate::util::AppError,
	app_error::{Context, app_error},
	std::{collections::HashSet, ffi::OsStr, fs, path::Path, sync::Arc},
	url::Url,
};

/// Entry image
#[derive(Clone)]
pub struct EntryImage {
	handle: egui::TextureHandle,
}

impl EntryImage {
	/// Creates a new entry image from an image
	pub fn new(egui_ctx: &egui::Context, path: &Path) -> Result<Self, AppError> {
		let mut image = image::open(path).context("Unable to read image")?;

		// Resize the image if it's too big for the gpu
		// Note: If the max texture size doesn't fit into a `u32`, then we can be sure
		//       that any image passed will fit.
		// TODO: Instead of decreasing the image size, can we just split into multiple images?
		let max_texture_size = egui_ctx.input(|input| input.max_texture_side);
		if let Ok(max_texture_size) = u32::try_from(max_texture_size) &&
			(image.width() > max_texture_size || image.height() > max_texture_size)
		{
			tracing::warn!(
				"Image size {}x{} did not fit into gpu's max texture size, resizing to fit \
				 {max_texture_size}x{max_texture_size}",
				image.width(),
				image.height()
			);
			image = image.resize(
				max_texture_size,
				max_texture_size,
				image::imageops::FilterType::Triangle,
			);
		}

		let image = egui::ColorImage::from_rgba_unmultiplied(
			[image.width() as usize, image.height() as usize],
			&image.into_rgba8().into_flat_samples().samples,
		);
		let image = egui::ImageData::Color(Arc::new(image));

		// TODO: This filter should be customizable.
		let options = egui::TextureOptions::LINEAR;
		// TODO: Could we make it so we don't require an egui context here?
		let handle = egui_ctx.load_texture(path.display().to_string(), image, options);

		Ok(Self { handle })
	}

	/// Creates a new thumbnail entry image from an image
	pub fn thumbnail(
		egui_ctx: &egui::Context,
		thumbnails_dir: &Path,
		video_exts: &HashSet<String>,
		path: &Path,
	) -> Result<Self, AppError> {
		let cache_path = {
			let path_absolute = path.canonicalize().context("Unable to canonicalize path")?;
			let path_uri = Url::from_file_path(&path_absolute)
				.map_err(|()| app_error!("Unable to turn path into url: {path_absolute:?}"))?;
			let path_md5 = md5::compute(path_uri.as_str());
			// TODO: Should we be using `png`s for the thumbnails?
			let thumbnail_file_name = format!("{path_md5:#x}.png");

			thumbnails_dir.join(thumbnail_file_name)
		};

		let image = match image::open(&cache_path) {
			Ok(image) => image,
			Err(err) => {
				// Note: `image` supports gifs, so we can still generate thumbnails for them
				let is_video = path
					.extension()
					.and_then(OsStr::to_str)
					.is_some_and(|ext| ext != "gif" && video_exts.contains(ext));

				match is_video {
					true => {
						tracing::debug!(
							?path,
							?cache_path,
							?err,
							"No thumbnail found, but path was a video, so skipping thumbnail"
						);

						// TODO: Implement video thumbnails
						image::DynamicImage::new_rgba8(256, 256)
					},
					false => {
						tracing::debug!(?path, ?cache_path, ?err, "No thumbnail found, generating one");
						let image = image::open(path).context("Unable to read image")?;
						let thumbnail = image.thumbnail(256, 256);

						// TODO: Make saving the thumbnail a non-fatal error
						fs::create_dir_all(thumbnails_dir).context("Unable to create thumbnails directory")?;
						thumbnail.save(cache_path).context("Unable to save thumbnail")?;

						thumbnail
					},
				}
			},
		};


		let image = egui::ColorImage::from_rgba_unmultiplied(
			[image.width() as usize, image.height() as usize],
			&image.into_rgba8().into_flat_samples().samples,
		);
		let image = egui::ImageData::Color(Arc::new(image));

		// TODO: This filter should be customizable.
		let options = egui::TextureOptions::LINEAR;
		// TODO: Could we make it so we don't require an egui context here?
		let handle = egui_ctx.load_texture(path.display().to_string(), image, options);

		Ok(Self { handle })
	}

	/// Returns the size of this image
	pub fn size(&self) -> egui::Vec2 {
		self.handle.size_vec2()
	}
}

impl core::fmt::Debug for EntryImage {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.debug_struct("EntryImage").field("handle", &self.handle.id()).finish()
	}
}

impl From<EntryImage> for egui::load::SizedTexture {
	fn from(image: EntryImage) -> Self {
		Self::from_handle(&image.handle)
	}
}
