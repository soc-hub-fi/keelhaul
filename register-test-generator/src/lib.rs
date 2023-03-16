//! Common types and functions for register test generator.

// TODO: leave error handling to customer crate

mod generate;
mod model;
mod parse_json;
mod parse_svd;

pub use generate::*;
pub use model::*;
pub use parse_json::*;
pub use parse_svd::*;

use std::{fs::File, num::ParseIntError, path::PathBuf, str::ParseBoolError};
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
pub enum JsonParseError {
    #[error("expected JSON object: {0}")]
    ExpectedObject(String),
    #[error("expected JSON array: {0}")]
    ExpectedArray(String),
    #[error("JSON object does not contain field for '{0}'")]
    FieldNotFound(String),
    #[error("could not parse int")]
    ParseInt(#[from] ParseIntError),
    #[error("could not parse bool")]
    ParseBool(#[from] ParseBoolError),
}

#[derive(Error, Debug)]
pub enum ParseError {
    #[error("expected field in node: {0}")]
    ExpectedTag(String),
    #[error("could not parse int")]
    ParseInt(#[from] ParseIntError),
    #[error("expected int: {0}")]
    InvalidInt(String),
    #[error("invalid size multiplier suffix: {0}")]
    InvalidSizeMultiplierSuffix(char),
    #[error("invalid access type: {0}")]
    InvalidAccessType(String),
}

#[derive(Debug)]
pub enum AddrRepr<T> {
    Comps { base: T, cluster: T, offset: T },
    Full(T),
}

#[derive(Error, Debug)]
pub enum GenerateError {
    #[error("generated address for {0} does not fit in a 64-bit pointer: {1:?}")]
    AddrOverflow(String, AddrRepr<u64>),
}
