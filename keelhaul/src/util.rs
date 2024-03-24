//! Methods for reading files

use std::path;

use fs_err as fs;

/// Returns contents of a file at `path`, panicking on any failure
///
/// # Panics
///
/// This function panics if the path does not exist, or if the file cannot be
/// read.
#[must_use]
pub fn read_file_or_panic(path: &path::Path) -> String {
    path.canonicalize().map_or_else(
        |err| panic!("path {} does not exist: {err}", path.display()),
        |p| {
            fs::read_to_string(p)
                .unwrap_or_else(|err| panic!("cannot read file at path {}: {err}", path.display()))
        },
    )
}
