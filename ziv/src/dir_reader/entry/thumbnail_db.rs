//! Thumbnail database

// TODO: Instead of accepting and returning byte slices, should we try to
//       use the blob API to read the images directly?

// Imports
use {
	crate::util::AppError,
	app_error::{Context, bail},
	image::{DynamicImage, ImageFormat, ImageReader},
	parking_lot::Mutex,
	rusqlite::{OptionalExtension, types::FromSqlError},
	std::{io, path::Path, sync::Arc},
	time::OffsetDateTime,
};

/// Thumbnail database
#[derive(Clone, Debug)]
pub struct ThumbnailDb {
	conn: Arc<Mutex<rusqlite::Connection>>,
}

impl ThumbnailDb {
	/// Creates or opens a thumbnail database from path `path`
	pub fn new(path: &Path) -> Result<Self, AppError> {
		let conn = rusqlite::Connection::open(path).context("Unable to open database")?;

		match conn
			.table_exists(None, "meta")
			.context("Unable to check if table exists")?
		{
			true =>
				#[expect(clippy::never_loop, reason = "It will loop once we add any migrations")]
				loop {
					let version = conn
						.query_one(
							r#"select "meta"."value" from "meta" where "meta"."key" = 'version'"#,
							(),
							|row| row.get::<_, usize>(0),
						)
						.context("Unable to query")?;

					match version {
						0 => break,
						_ => bail!("Unknown version {version:?}"),
					}
				},
			false => conn
				.execute_batch(include_str!("thumbnail_db/sql/schema.sql"))
				.context("Unable to initialize database")?,
		}

		Ok(Self {
			conn: Arc::new(Mutex::new(conn)),
		})
	}

	/// Gets a thumbnail image, if it exists
	pub fn thumbnail_image(&self, path: &Path) -> Result<Option<(OffsetDateTime, DynamicImage)>, AppError> {
		let Some((date_added, format, bytes)) = self.thumbnail(path)? else {
			return Ok(None);
		};
		let image = ImageReader::with_format(io::Cursor::new(bytes), format)
			.decode()
			.context("Unable to decode thumbnail")?;

		Ok(Some((date_added, image)))
	}

	/// Gets a thumbnail, if it exists
	pub fn thumbnail(&self, path: &Path) -> Result<Option<(OffsetDateTime, ImageFormat, Vec<u8>)>, AppError> {
		let conn = self.conn.lock();
		conn.query_one(
			include_str!("thumbnail_db/sql/get_thumbnail.sql"),
			rusqlite::named_params! {
				":path": path.to_str().context("Path was non-utf8")?,
			},
			|row| {
				let format_ext = row.get::<_, String>(1)?;
				let format = ImageFormat::from_extension(&format_ext).ok_or_else(|| {
					FromSqlError::Other(format!("Unknown image format extension {format_ext:?}").into())
				})?;

				Ok((row.get(0)?, format, row.get(2)?))
			},
		)
		.optional()
		.context("Unable to query database")
	}

	/// Inserts a thumbnail from an image.
	///
	/// Returns the size of the image saved
	pub fn add_thumbnail_image(
		&self,
		path: &Path,
		format: ImageFormat,
		image: &DynamicImage,
	) -> Result<usize, AppError> {
		let mut bytes = vec![];
		image
			.write_to(io::Cursor::new(&mut bytes), format)
			.context("Unable to write thumbnail")?;

		let date_added = OffsetDateTime::now_utc();
		self.add_thumbnail(path, date_added, format, &bytes)?;

		Ok(bytes.len())
	}

	/// Inserts a thumbnail
	pub fn add_thumbnail(
		&self,
		path: &Path,
		date_added: OffsetDateTime,
		format: ImageFormat,
		thumbnail: &[u8],
	) -> Result<(), AppError> {
		let conn = self.conn.lock();
		conn.execute(
			include_str!("thumbnail_db/sql/add_thumbnail.sql"),
			rusqlite::named_params! {
				":path": path.to_str().context("Path was non-utf8")?,
				":date_added": date_added,
				":format_ext": format.extensions_str()[0],
				":thumbnail": thumbnail,
			},
		)
		.context("Unable to execute in database")?;

		Ok(())
	}
}
