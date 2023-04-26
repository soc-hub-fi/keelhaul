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

use std::{
    env,
    fs::{self, File},
    path::{self, PathBuf},
};

/// Check that path to a file exists.
///
/// # Panics
///
/// This function panics if the path does not exist.
pub fn validate_path_existence(path_str: &str) -> PathBuf {
    match PathBuf::from(path_str).canonicalize() {
        Ok(path) => match path.try_exists() {
            Ok(exists) => {
                assert!(exists, "Path {} does not exist.", path.display());
                path
            }
            Err(error) => panic!("Path {} does not exist. {}", path.display(), error),
        },
        Err(error) => panic!("Path {path_str} does not exist. {error}"),
    }
}

/// Try to extract path to excludes-file from environment variable.
fn read_excludes_path_from_env() -> Option<PathBuf> {
    env::var("PATH_EXCLUDES")
        .ok()
        .map(|p| validate_path_existence(&p))
}

/// Try to get names of excluded registers.
fn read_excludes_from_env() -> Option<Vec<String>> {
    read_excludes_path_from_env().map(|path| {
        let content = fs::read_to_string(path).expect("Failed to read excludes content.");
        let registers = content.split('\n').map(ToOwned::to_owned).collect_vec();
        registers
    })
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
    fn list(white_list: Option<Vec<T>>, block_list: Vec<T>) -> ItemFilter<T> {
        Self::List {
            white_list,
            block_list,
        }
    }

    fn regex(allow: Option<Regex>, block: Option<Regex>) -> ItemFilter<T> {
        Self::Regex { allow, block }
    }

    fn is_allowed(&self, value: &T) -> bool
    where
        T: AsRef<str>,
    {
        match self {
            ItemFilter::List {
                white_list,
                block_list,
            } => {
                // Items in block list are always blocked
                if block_list.contains(value) {
                    return false;
                }

                match &white_list {
                    Some(white_list) => white_list.contains(value),
                    None => true,
                }
            }
            ItemFilter::Regex { allow, block } => {
                // Items matched by block regex are always blocked
                if let Some(block) = block {
                    if block.is_match(value.as_ref()) {
                        return false;
                    }
                }

                if let Some(allow) = allow {
                    allow.is_match(value.as_ref())
                } else {
                    true
                }
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

/// Read an environment variable into a Vec<String>
///
/// # Parameters:
///
/// `var` - The name of the environment variable
/// `sep` - The separator for Vec elements
///
/// Returns Some(`v`) if the variable is present, None otherwise
fn read_vec_from_env(var: &str, sep: char) -> Option<Vec<String>> {
    if let Ok(included_str) = env::var(var) {
        let peripherals = included_str.split(sep).map(ToOwned::to_owned).collect_vec();
        // TODO: validate that these are valid peripherals
        Some(peripherals)
    } else {
        None
    }
}

/// Read the input SVD to string
fn read_to_string(fpath: &path::Path) -> String {
    if !fpath.exists() {
        panic!("SVD was not found at {}", fpath.display());
    }
    fs::read_to_string(fpath).unwrap()
}
