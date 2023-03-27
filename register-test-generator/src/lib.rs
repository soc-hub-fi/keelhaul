//! Common types and functions for register test generator.

// TODO: leave error handling to customer crate

mod generate;
mod model;
mod parse_svd;

pub use generate::*;
pub use model::*;
pub use parse_svd::*;

use std::{fs::File, num::ParseIntError, path::PathBuf};
use thiserror::Error;
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

/// Checks if path to a file exists, and create it if it does not exist.
///
/// # Panics
///
/// This function panics if path can not be accessed.
/// This can happen if the operating system denies access to the path.
pub fn get_or_create(path_str: &str) -> PathBuf {
    match PathBuf::from(path_str).canonicalize() {
        Ok(path) => match path.try_exists() {
            Ok(exists) => {
                assert!(exists, "Path {} does not exist.", path.display());
                path
            }
            Err(error) => panic!("Path {} does not exist. {}", path.display(), error),
        },
        Err(error) => {
            println!("Path {path_str} does not exist. {error}");
            match File::create(path_str) {
                Ok(_file) => {
                    println!("Created new file to path {path_str}.");
                    validate_path_existence(path_str)
                }
                Err(error) => panic!("Failed to create new file to path {path_str}. {error}"),
            }
        }
    }
}

/// Error that happened during parsing 'CMSIS-SVD' or 'IP-XACT'
#[derive(Error, Debug)]
pub enum CommonParseError {
    #[error("invalid access type in input: {0}")]
    InvalidAccessType(String),
}

#[derive(Error, Debug)]
#[error("address for {0} does not fit in architecture pointer {1:?}")]
pub struct AddrOverflowError(String, AddrRepr<u64>);

#[derive(Error, Debug)]
pub enum Error {
    #[error("error while parsing SVD")]
    SvdParse(#[from] SvdParseError),
    #[error("error while compiling regex")]
    Regex(#[from] regex::Error),
}

/// Error that happened during parsing 'CMSIS-SVD'
#[derive(Error, Debug)]
pub enum SvdParseError {
    #[error("expected tag {tag:?} in element {elem_name:?}")]
    ExpectedTagInElement { elem_name: String, tag: String },
    #[error("could not parse int")]
    ParseInt(#[from] ParseIntError),
    #[error("could not parse nonneg int from {0}")]
    InvalidNonnegInt(String),
    #[error("invalid size multiplier suffix: {0}")]
    InvalidSizeMultiplierSuffix(char),
    #[error("failed to convert {0} bits into a valid pointer width, must be multiple of 8")]
    BitCountToPtrWidth(u64),
    #[error("not implemented")]
    NotImplemented(#[from] NotImplementedError),
    #[error("parsed address overflows")]
    AddrOverflow(#[from] AddrOverflowError),
    #[error("generic parse error")]
    GenericParse(#[from] CommonParseError),
    #[error("invalid CMSIS-SVD protection type: {0}")]
    InvalidProtectionType(String),
}

#[derive(Error, Debug)]
pub enum NotImplementedError {
    #[error(
        "detected SVD register array: '{0}' but arrays are not yet implemented by test generator"
    )]
    SvdArray(String),
}

/// Error that happened during test case generation.
#[derive(Error, Debug)]
pub enum GenerateError {
    #[error("generated address overflows")]
    AddrOverflow(#[from] AddrOverflowError),
}
