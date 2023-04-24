//! Common types and functions for register test generator.

// TODO: leave error handling to customer crate

mod generate;
mod model;
mod parse_svd;

pub use generate::*;
pub use model::*;
pub use parse_svd::*;

use std::{fmt, fs::File, num::ParseIntError, path::PathBuf};
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
pub struct AddrOverflowError<T: num::CheckedAdd>(String, AddrRepr<T>);

#[derive(Error, Debug)]
pub enum Error {
    #[error("error while parsing SVD")]
    SvdParse(#[from] SvdParseError),
    #[error("error while compiling regex")]
    Regex(#[from] regex::Error),
    #[error("zero entries were chosen from SVD, either the file doesn't have any register definitions, or they were all ignored by current flags")]
    ZeroEntries,
}

#[derive(Error, Debug)]
#[error("CMSIS-SVD parse error --> {fname}:{err}")]
pub struct ParseFileError<T> {
    fname: String,
    err: PositionalError<T>,
}

/// Representation of a file position in an error
///
/// Indexes start from 1:1.
#[derive(Debug)]
pub(crate) enum Position {
    Point {
        line: u32,
        col: u32,
    },
    Line {
        line: u32,
        start_col: u32,
        end_col: u32,
    },
    MultiLine {
        start_line: u32,
        start_col: u32,
        end_line: u32,
        end_col: u32,
    },
}

impl From<ops::Range<roxmltree::TextPos>> for Position {
    fn from(value: ops::Range<roxmltree::TextPos>) -> Self {
        if value.start.row == value.end.row {
            // Same line, same column --> Point
            if value.start.col == value.end.col {
                Position::Point {
                    line: value.start.row,
                    col: value.start.col,
                }
            }
            // Same line but different column --> Line
            else {
                Position::Line {
                    line: value.start.row,
                    start_col: value.start.col,
                    end_col: value.end.col,
                }
            }
        }
        // Starts and ends on different lines --> MultiLine
        else {
            Position::MultiLine {
                start_line: value.start.row,
                start_col: value.start.col,
                end_line: value.end.row,
                end_col: value.end.col,
            }
        }
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Position::Point { line, col } => write!(f, "{line}:{col}"),
            Position::Line {
                line,
                start_col,
                end_col,
            } => write!(f, "{line}:{start_col}-{end_col}"),
            Position::MultiLine {
                start_line,
                start_col,
                end_line,
                end_col,
            } => write!(f, "{start_line}:{start_col}..{end_line}:{end_col}"),
        }
    }
}

#[derive(Error, Debug)]
#[error("{pos}\n{err}")]
pub struct PositionalError<T> {
    pos: Position,
    err: T,
}

impl<T> PositionalError<T> {
    pub(crate) fn with_fname(self, fname: String) -> ParseFileError<T> {
        ParseFileError { fname, err: self }
    }
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
    #[error("parsed 32-bit address overflows")]
    AddrOverflow32(#[from] AddrOverflowError<u32>),
    #[error("parsed 64-bit address overflows")]
    AddrOverflow64(#[from] AddrOverflowError<u64>),
    #[error("generic parse error")]
    GenericParse(#[from] CommonParseError),
    #[error("invalid CMSIS-SVD protection type: {0}")]
    InvalidProtectionType(String),
    #[error("register reset value and mask are of different types")]
    ResetValueMaskTypeMismatch(#[from] IncompatibleTypesError),
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
    #[error("generated 32-bit address overflows")]
    AddrOverflow32(#[from] AddrOverflowError<u32>),
    #[error("generated 64-bit address overflows")]
    AddrOverflow64(#[from] AddrOverflowError<u64>),
    #[error("invalid configuration: {cause}, {c:#?}")]
    InvalidConfig { c: TestConfig, cause: String },
}
