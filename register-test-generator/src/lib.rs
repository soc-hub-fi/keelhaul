//! Common types and functions for register test generator.

// TODO: leave error handling to customer crate

mod error;
mod generate;
mod model;
mod parse_svd;

pub use error::*;
pub use generate::*;
use itertools::Itertools;
pub use model::*;
pub use parse_svd::*;
use regex::Regex;

use fs_err as fs;
use std::{
    env,
    path::{Path, PathBuf},
};

/// Returns contents of a file at `path`, panicking on any failure
///
/// # Panics
///
/// This function panics if the path does not exist, or if the file cannot be
/// read.
#[must_use]
pub fn read_file_or_panic(path: &Path) -> String {
    path.canonicalize().map_or_else(
        |err| panic!("path {} does not exist: {err}", path.display()),
        |p| {
            fs::read_to_string(p)
                .unwrap_or_else(|err| panic!("cannot read file at path {}: {err}", path.display()))
        },
    )
}

/// Try to extract path to excludes-file from environment variable.
fn read_file_from_env_or_panic(var: &str) -> Option<String> {
    env::var(var)
        .ok()
        .map(|p| read_file_or_panic(&PathBuf::from(p)))
}

/// Try to get names of excluded registers.
fn read_excludes_from_env() -> Option<Vec<String>> {
    read_file_from_env_or_panic("PATH_EXCLUDES").map(|contents|
            // One register per line
            contents.split('\n').map(ToOwned::to_owned).collect_vec())
}

/// What items of type `T` are allowed or not
enum ItemFilter<T: PartialEq> {
    List {
        // If set, only the specified items are allowed. If not set, all items are
        // allowed except the ones listed in blocklist.
        white_list: Option<Vec<T>>,
        // These items are always blocked even if present in `white_list`
        block_list: Vec<T>,
    },
    Regex {
        allow: Option<Regex>,
        block: Option<Regex>,
    },
}

impl<T: PartialEq> ItemFilter<T> {
    #[allow(clippy::use_self)]
    fn list(white_list: Option<Vec<T>>, block_list: Vec<T>) -> ItemFilter<T> {
        Self::List {
            white_list,
            block_list,
        }
    }

    #[allow(clippy::use_self)]
    const fn regex(allow: Option<Regex>, block: Option<Regex>) -> ItemFilter<T> {
        Self::Regex { allow, block }
    }

    fn is_allowed(&self, value: &T) -> bool
    where
        T: AsRef<str>,
    {
        match self {
            Self::List {
                white_list,
                block_list,
            } => {
                // Items in block list are always blocked
                if block_list.contains(value) {
                    return false;
                }
                white_list.as_ref().map_or(true, |wl| wl.contains(value))
            }
            Self::Regex { allow, block } => {
                // Items matched by block regex are always blocked
                if let Some(block) = block {
                    if block.is_match(value.as_ref()) {
                        return false;
                    }
                }
                allow
                    .as_ref()
                    .map_or(true, |allow| allow.is_match(value.as_ref()))
            }
        }
    }

    fn is_blocked(&self, value: &T) -> bool
    where
        T: AsRef<str>,
    {
        !self.is_allowed(value)
    }
}

/// Returns a vector containing elements read from environment variable `var` if
/// the variable is present.
///
/// # Parameters:
///
/// `var` - The name of the environment variable to be read
/// `sep` - The separator for Vec elements
fn read_vec_from_env(var: &str, sep: char) -> Option<Vec<String>> {
    env::var(var)
        .map(|s| {
            let peripherals = s.split(sep).map(ToOwned::to_owned).collect_vec();
            // TODO: validate that these are valid peripherals
            peripherals
        })
        .ok()
}
