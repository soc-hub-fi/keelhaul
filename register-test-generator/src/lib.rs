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

#[derive(Error, Debug)]
pub enum ParseError {
    #[error("invalid access type in input: {0}")]
    InvalidAccessType(String),
}

#[derive(Error, Debug)]
pub enum SvdParseError {
    #[error("expected field in node: {0}")]
    ExpectedTag(String),
    #[error("could not parse int")]
    ParseInt(#[from] ParseIntError),
    #[error("expected int: {0}")]
    InvalidInt(String),
    #[error("invalid size multiplier suffix: {0}")]
    InvalidSizeMultiplierSuffix(char),
    #[error("failed to convert {0} bits into a valid pointer width")]
    BitCountToPtrWidth(u64),
    #[error("not implemented")]
    NotImplemented(#[from] NotImplementedError),
    #[error("address for {0} does not fit in architecture pointer: {1:?}")]
    AddrOverflow(String, AddrRepr<u64>),
    #[error("generic parse error")]
    GenericParse(#[from] ParseError),
}

#[derive(Error, Debug)]
pub enum NotImplementedError {
    #[error(
        "detected SVD register array: '{0}' but arrays are not yet implemented by test generator"
    )]
    SvdArray(String),
}

#[derive(Error, Debug)]
pub enum GenerateError {
    #[error("generated address for {0} does not fit in architecture pointer: {1:?}")]
    AddrOverflow(String, AddrRepr<u64>),
}
