use std::{fmt, ops};

use crate::{AddrRepr, IncompatibleTypesError, TestConfig};
use thiserror::Error;

/// Error that happened during parsing 'CMSIS-SVD' or 'IP-XACT'
#[derive(Error, Debug)]
pub enum CommonParseError {
    #[error("invalid access type in input: {0}")]
    InvalidAccessType(String),
}

#[derive(Error, Debug)]
#[error("address for {0} does not fit in architecture pointer {1}")]
pub struct AddrOverflowError<T: num::CheckedAdd>(String, AddrRepr<T>);

impl<T: num::CheckedAdd> AddrOverflowError<T> {
    pub fn new(id: String, addr: AddrRepr<T>) -> Self {
        Self(id, addr)
    }
}

#[derive(Error, Debug)]
pub enum Error {
    #[error("error while parsing SVD")]
    SvdParse(#[from] ParseFileError<SvdParseError>),
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
    ParseInt(#[from] std::num::ParseIntError),
    #[error("could not parse nonneg int from {0}")]
    InvalidNonnegInt(String),
    #[error("invalid size multiplier suffix: {0}")]
    InvalidSizeMultiplierSuffix(char),
    #[error("failed to convert {0} bits into a valid pointer width, must be multiple of 8")]
    BitCountToPtrWidth(u64),
    #[error("not implemented")]
    NotImplemented(#[from] NotImplementedError),
    #[error("parsed 32-bit address overflows\n{0}")]
    AddrOverflow32(#[from] AddrOverflowError<u32>),
    #[error("parsed 64-bit address overflows\n{0}")]
    AddrOverflow64(#[from] AddrOverflowError<u64>),
    #[error("generic parse error")]
    GenericParse(#[from] CommonParseError),
    #[error("invalid CMSIS-SVD protection type: {0}")]
    InvalidProtectionType(String),
    #[error("register reset value and mask are of different types")]
    ResetValueMaskTypeMismatch(#[from] IncompatibleTypesError),
}

impl SvdParseError {
    /// Convert into positional error, adding row and column information
    pub(crate) fn with_text_pos_range(
        self,
        pos: ops::Range<roxmltree::TextPos>,
    ) -> PositionalError<SvdParseError> {
        PositionalError {
            pos: pos.into(),
            err: self,
        }
    }
    pub(crate) fn with_byte_pos_range(
        self,
        byte_pos: ops::Range<usize>,
        doc: &roxmltree::Document,
    ) -> PositionalError<SvdParseError> {
        let text_pos = ops::Range {
            start: doc.text_pos_at(byte_pos.start),
            end: doc.text_pos_at(byte_pos.end),
        };
        self.with_text_pos_range(text_pos)
    }
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
