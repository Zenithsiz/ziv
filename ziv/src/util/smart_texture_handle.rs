//! Smart texture handle

use {
	super::{EguiCtxLoadImage, EguiTextureHandle},
	image::DynamicImage,
	std::sync::Arc,
};

/// Smart texture handle.
///
/// This texture handle wrapper supports creating and rendering images larger
/// than the maximum texture size of the gpu by tiling them.
#[derive(Clone, Debug)]
pub enum SmartTextureHandle {
	Simple(EguiTextureHandle),

	/// Invariants:
	///
	/// - `tiles.len() == width * height`
	/// - Each non-right-most tile has width of the maximum texture size
	/// - Each non-bottom-most tile has height of the maximum texture size
	Tiled {
		tiles_width:  usize,
		tiles_height: usize,
		tiles:        Arc<[EguiTextureHandle]>,
	},
}

impl SmartTextureHandle {
	/// Creates a new texture from an image
	pub fn new(name: String, image: DynamicImage, egui_ctx: &egui::Context) -> Self {
		// Split the image if it's too big for the gpu
		// Note: If the max texture size doesn't fit into a `u32`, then we can be sure
		//       that any image passed will fit.
		// TODO: For debugging allow this size to be changed by getting it from elsewhere.
		let max_texture_size = egui_ctx.input(|input| input.max_texture_side);
		if let Ok(max_texture_size) = u32::try_from(max_texture_size) &&
			(image.width() > max_texture_size || image.height() > max_texture_size)
		{
			return Self::new_split(&name, image, egui_ctx, max_texture_size);
		}

		let handle = egui_ctx.load_image(name, image);
		Self::Simple(EguiTextureHandle(handle))
	}

	/// Creates the image's texture by splitting the image
	fn new_split(name: &str, mut image: DynamicImage, egui_ctx: &egui::Context, max_texture_size: u32) -> Self {
		let tiles_width = image.width().div_ceil(max_texture_size);
		let tiles_height = image.height().div_ceil(max_texture_size);

		tracing::debug!(
			"Image size {}x{} did not fit into gpu's max texture size {max_texture_size}x{max_texture_size}, \
			 splitting it into {tiles_width}x{tiles_height}",
			image.width(),
			image.height()
		);

		let mut tiles = vec![];
		for tile_y in 0..tiles_height {
			let y = tile_y * max_texture_size;
			let height = match tile_y + 1 == tiles_height {
				true => image.height().rem_euclid(max_texture_size),
				false => max_texture_size,
			};

			for tile_x in 0..tiles_width {
				let x = tile_x * max_texture_size;
				let width = match tile_x + 1 == tiles_width {
					true => image.width().rem_euclid(max_texture_size),
					false => max_texture_size,
				};

				// TODO: We can be more efficient than this as cropping, especially
				//       when our width is 1.
				let tile_image = image.crop(x, y, width, height);
				let handle = egui_ctx.load_image(format!("{name}@{x}+{y}"), tile_image);
				tiles.push(EguiTextureHandle(handle));
			}
		}

		Self::Tiled {
			tiles_width:  tiles_width as usize,
			tiles_height: tiles_height as usize,
			tiles:        tiles.into(),
		}
	}
}

impl SmartTextureHandle {
	pub fn size(&self) -> [usize; 2] {
		match *self {
			Self::Simple(ref handle) => handle.size(),
			Self::Tiled {
				tiles_width,
				tiles_height,
				ref tiles,
			} => {
				// TODO: Simplify this
				let mut size = [0; 2];
				for x in 0..tiles_width {
					let y = 0;
					let tile_size = tiles[y * tiles_width + x].size();
					size[0] += tile_size[0];
				}
				for y in 0..tiles_height {
					let x = 0;
					let tile_size = tiles[y * tiles_width + x].size();
					size[1] += tile_size[1];
				}

				size
			},
		}
	}

	pub fn size_vec2(&self) -> egui::Vec2 {
		let [width, height] = self.size();
		egui::vec2(width as f32, height as f32)
	}

	/// Paints this widget inside a rect
	// TODO: Create a builder for this?
	pub fn paint_at(&self, ui: &egui::Ui, uv_rect: egui::Rect, ui_rect: egui::Rect) {
		match *self {
			Self::Simple(ref handle) => egui::Image::new(handle).uv(uv_rect).paint_at(ui, ui_rect),

			// TODO: When using a linear filter, zooming in reveals the edges
			//       between images. Solve it by cropping the images 1 pixel
			//       bigger than necessary and overdrawing here?
			Self::Tiled {
				tiles_width,
				tiles_height,
				ref tiles,
			} => {
				if tiles_width == 0 || tiles_height == 0 {
					return;
				}

				let total_uv_rect = uv_rect;
				let total_ui_rect = ui_rect;

				let max_texture_size = ui.input(|input| input.max_texture_side);

				// Total size
				let [total_width, total_height] = self.size();

				// Size of the last tiles.
				let last_width = total_width.rem_euclid(max_texture_size);
				let last_height = total_height.rem_euclid(max_texture_size);

				// Tiles to start (inclusive) and end (exclusive) drawing at
				let start_tile_x =
					(total_uv_rect.min.x * total_width as f32 / max_texture_size as f32).floor() as usize;
				let start_tile_y =
					(total_uv_rect.min.y * total_height as f32 / max_texture_size as f32).floor() as usize;

				let end_tile_x = (total_uv_rect.max.x * total_width as f32 / max_texture_size as f32).ceil() as usize;
				let end_tile_x = end_tile_x.min(tiles_width);

				let end_tile_y = (total_uv_rect.max.y * total_height as f32 / max_texture_size as f32).ceil() as usize;
				let end_tile_y = end_tile_y.min(tiles_height);

				// Offset of the top-left tile's image (from the top-left of the tile)
				let global_start_x = start_tile_x * max_texture_size;
				let global_start_y = start_tile_y * max_texture_size;
				let start_pos = egui::vec2(
					total_uv_rect.min.x * total_width as f32 - global_start_x as f32,
					total_uv_rect.min.y * total_height as f32 - global_start_y as f32,
				);

				// Offset of the bottom-right tile's image (from the bottom-right of the tile)
				let global_end_x = match end_tile_x == tiles_width {
					true => (end_tile_x - 1) * max_texture_size + last_width,
					false => end_tile_x * max_texture_size,
				};
				let global_end_y = match end_tile_y == tiles_height {
					true => (end_tile_y - 1) * max_texture_size + last_height,
					false => end_tile_y * max_texture_size,
				};
				let end_pos = egui::vec2(
					global_end_x as f32 - total_uv_rect.max.x * total_width as f32,
					global_end_y as f32 - total_uv_rect.max.y * total_height as f32,
				);

				// Size of the on-screen image
				let slice_width = {
					let tiles_width = match end_tile_x == tiles_width {
						true => (end_tile_x - 1 - start_tile_x) * max_texture_size + last_width,
						false => (end_tile_x - start_tile_x) * max_texture_size,
					};
					-start_pos.x + tiles_width as f32 - end_pos.x
				};
				let slice_height = {
					let tiles_height = match end_tile_y == tiles_height {
						true => (end_tile_y - 1 - start_tile_y) * max_texture_size + last_height,
						false => (end_tile_y - start_tile_y) * max_texture_size,
					};
					-start_pos.y + tiles_height as f32 - end_pos.y
				};
				let slice_size = egui::vec2(slice_width, slice_height);

				for y in start_tile_y..end_tile_y {
					for x in start_tile_x..end_tile_x {
						let tile = &tiles[y * tiles_width + x];

						// Size of the tile
						let tile_width = match x + 1 == tiles_width {
							true => last_width,
							false => max_texture_size,
						};
						let tile_height = match y + 1 == tiles_height {
							true => last_height,
							false => max_texture_size,
						};

						// Ui tile position
						let pos_offset = -start_pos / slice_size * total_ui_rect.size();
						let ui_pos = total_ui_rect.min +
							pos_offset + egui::vec2((x - start_tile_x) as f32, (y - start_tile_y) as f32) *
							max_texture_size as f32 /
							slice_size * total_ui_rect.size();

						// Ui tile size
						let ui_size = egui::vec2(
							(tile_width as f32 / slice_width) * total_ui_rect.width(),
							(tile_height as f32 / slice_height) * total_ui_rect.height(),
						);

						let mut uv_rect = egui::Rect::from_min_size(egui::Pos2::ZERO, egui::Vec2::ONE);
						let mut ui_rect = egui::Rect::from_min_size(ui_pos, ui_size);

						// Get rid of any ui rects that fall out of the original ui rect
						// TODO: Do these adjustments above to avoid mutating the rectangles after.
						if ui_rect.min.x < total_ui_rect.min.x || ui_rect.min.y < total_ui_rect.min.y {
							let delta = (total_ui_rect.min - ui_rect.min).max(egui::Vec2::ZERO);
							uv_rect.min += delta / ui_size;
							ui_rect.min += delta;
						}
						if ui_rect.max.x > total_ui_rect.max.x || ui_rect.max.y > total_ui_rect.max.y {
							let delta = (total_ui_rect.max - ui_rect.max).min(egui::Vec2::ZERO);
							uv_rect.max += delta / ui_size;
							ui_rect.max += delta;
						}

						egui::Image::new(tile).uv(uv_rect).paint_at(ui, ui_rect);
					}
				}
			},
		}
	}
}
