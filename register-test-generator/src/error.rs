use std::{
    convert, fmt,
    ops::{self, RangeInclusive},
};

use crate::{AddrRepr, ArchiPtr, IncompatibleTypesError, TestConfig};
use thiserror::Error;

/// Error that happened during parsing 'CMSIS-SVD' or 'IP-XACT'
#[derive(Error, Debug, Clone)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub enum CommonParseError {
    #[error("invalid access type in input: {0}")]
    InvalidAccessType(String),
}

#[derive(Error, Debug)]
#[cfg_attr(test, derive(PartialEq, Eq))]
#[error("address for {0} does not fit in architecture pointer {1}")]
pub struct AddrOverflowError<T: num::CheckedAdd>(String, AddrRepr<T>);

impl<T: num::CheckedAdd> AddrOverflowError<T> {
    pub const fn new(id: String, addr: AddrRepr<T>) -> Self {
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
    #[error("missing environment variable: {0}")]
    MissingEnvironmentVariable(String),
    #[error("pointer size of {0} bits is not supported")]
    PointerSizeNotSupported(usize),
    #[error(
        "SVD-file must contain {}..={} {}-nodes, contained {}", expected_count.start(), expected_count.end(), node_name, actual_count
    )]
    InvalidNodeCount {
        node_name: String,
        expected_count: RangeInclusive<usize>,
        actual_count: usize,
    },
}

#[derive(Error, Debug)]
#[error("CMSIS-SVD parse error --> {fname}:{err}")]
pub struct ParseFileError<T>
where
    T: Clone,
{
    fname: String,
    err: PositionalError<T>,
}

/// Representation of a file position in an error
///
/// Indexes start from 1:1.
#[derive(Debug)]
pub enum Position {
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
                Self::Point {
                    line: value.start.row,
                    col: value.start.col,
                }
            }
            // Same line but different column --> Line
            else {
                Self::Line {
                    line: value.start.row,
                    start_col: value.start.col,
                    end_col: value.end.col,
                }
            }
        }
        // Starts and ends on different lines --> MultiLine
        else {
            Self::MultiLine {
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
            Self::Point { line, col } => write!(f, "{line}:{col}"),
            Self::Line {
                line,
                start_col,
                end_col,
            } => write!(f, "{line}:{start_col}-{end_col}"),
            Self::MultiLine {
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
pub struct PositionalError<T>
where
    T: Clone,
{
    pos: Position,
    err: T,
}

impl<T> PositionalError<T>
where
    T: Clone,
{
    pub(crate) const fn with_fname(self, fname: String) -> ParseFileError<T> {
        ParseFileError { fname, err: self }
    }

    pub(crate) fn error(&self) -> T {
        self.err.clone()
    }
}

/// Error that happened during parsing 'CMSIS-SVD'
#[derive(Error, Debug, Clone)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub enum SvdParseError {
    #[error("expected tag {tag:?} in element {elem_name:?}")]
    ExpectedTagInElement { elem_name: String, tag: String },
    #[error("could not parse int: {0}")]
    ParseInt(#[from] std::num::ParseIntError),
    #[error("could not parse nonneg int from {0}")]
    InvalidNonnegInt(String),
    #[error("invalid size multiplier suffix: {0}")]
    InvalidSizeMultiplierSuffix(char),
    #[error("failed to convert {0} bits into a valid pointer width, must be multiple of 8")]
    BitCountToPtrWidth(u64),
    #[error("not implemented")]
    NotImplemented(#[from] NotImplementedError),
    #[error("generic parse error")]
    GenericParse(#[from] CommonParseError),
    #[error("invalid CMSIS-SVD protection type: {0}")]
    InvalidProtectionType(String),
    #[error("register reset value and mask are of different types")]
    ResetValueMaskTypeMismatch(#[from] IncompatibleTypesError),
    #[error("internal error: infallible error should not be constructible")]
    Infallible(#[from] convert::Infallible),
    #[error("could not convert from int: {0}")]
    TryFromInt(#[from] std::num::TryFromIntError),
    #[error(
        "SVD-file must contain {}..={} {}-nodes, contained {}", expected_count.start(), expected_count.end(), node_name, actual_count
    )]
    InvalidNodeCount {
        node_name: String,
        expected_count: RangeInclusive<usize>,
        actual_count: usize,
    },
    #[error("missing environment variable: {0}")]
    MissingEnvironmentVariable(String),
    #[error("pointer size of {0} bits is not supported")]
    PointerSizeNotSupported(usize),
}

impl SvdParseError {
    /// Convert into positional error, adding row and column information
    pub(crate) fn with_text_pos_range(
        self,
        pos: ops::Range<roxmltree::TextPos>,
    ) -> PositionalError<Self> {
        PositionalError {
            pos: pos.into(),
            err: self,
        }
    }
    pub(crate) fn with_byte_pos_range(
        self,
        byte_pos: ops::Range<usize>,
        doc: &roxmltree::Document,
    ) -> PositionalError<Self> {
        let text_pos = ops::Range {
            start: doc.text_pos_at(byte_pos.start),
            end: doc.text_pos_at(byte_pos.end),
        };
        self.with_text_pos_range(text_pos)
    }
}

#[derive(Error, Debug, Clone)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub enum NotImplementedError {
    #[error(
        "detected SVD register array: '{0}' but arrays are not yet implemented by test generator"
    )]
    SvdArray(String),
    #[error("generating tests for a platform with a {0}-byte pointer is not (yet?) supported")]
    PtrSize(u8),
}

/// Error that happened during test case generation.
#[derive(Error, Debug)]
pub enum GenerateError {
    #[error("generated address overflows {archi_bits}-bit architecture pointer\n{err}")]
    AddrOverflow {
        err: Box<dyn std::error::Error + Send + Sync>,
        archi_bits: u8,
    },
    #[error("invalid configuration: {cause}, {c:#?}")]
    InvalidConfig { c: TestConfig, cause: String },
}

impl<P: ArchiPtr + 'static> From<AddrOverflowError<P>> for GenerateError {
    fn from(value: AddrOverflowError<P>) -> Self {
        Self::AddrOverflow {
            err: Box::new(value),
            archi_bits: P::ptr_size().bits(),
        }
    }
}
