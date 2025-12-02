//! Arguments

// Imports
use std::path::PathBuf;

/// Rust-tidy formatter
#[derive(Debug)]
#[derive(clap::Parser)]
#[clap(author, version, about)]
pub struct Args {
	/// Logs output to a file.
	///
	/// You can use `RUST_FILE_LOG` to set filtering options
	#[clap(long = "log-file")]
	pub log_file: Option<PathBuf>,

	/// Configuration file to use.
	#[clap(long = "config-file")]
	pub config_file: Option<PathBuf>,

	/// Path to open (or current if none given)
	pub path: Option<PathBuf>,
}
