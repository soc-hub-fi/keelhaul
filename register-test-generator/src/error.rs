use std::{convert, fmt, ops};

use crate::{AddrRepr, ArchiPtr, IncompatibleTypesError, TestConfig};
use thiserror::Error;

/// Error that happened during parsing 'CMSIS-SVD' or 'IP-XACT'
#[derive(Error, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum CommonParseError {
    #[error("invalid access type in input: {0}")]
    InvalidAccessType(String),
    #[error("expected tag {tag:?} in element {elem_name:?}")]
    ExpectedTagInElement { elem_name: String, tag: String },
}

impl CommonParseError {
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

#[derive(Error, Debug)]
#[cfg_attr(test, derive(PartialEq))]
#[error("address for {0} does not fit in architecture pointer {1}")]
pub struct AddrOverflowError<T: num::CheckedAdd>(String, AddrRepr<T>);

impl<T: num::CheckedAdd> AddrOverflowError<T> {
    pub fn new(id: String, addr: AddrRepr<T>) -> Self {
        Self(id, addr)
    }
}

/// Error that happened during parsing program arguments
#[derive(Error, Debug)]
pub enum ProgramArgumentError {
    #[error("no input file was given")]
    NoInputFileGiven,
}

#[derive(Error, Debug)]
pub enum Error {
    #[error("error while parsing SVD")]
    SvdParse(#[from] ParseFileError<SvdParseError>),
    #[error("error while parsing IP-XACT")]
    IpxactParse(#[from] ParseFileError<IpxactParseError>),
    #[error("error while compiling regex")]
    Regex(#[from] regex::Error),
    #[error("zero entries were chosen from SVD, either the file doesn't have any register definitions, or they were all ignored by current flags")]
    ZeroEntries,
    #[error("error while processing program arguments")]
    ProgramArgument(#[from] ProgramArgumentError),
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
    pub(crate) pos: Position,
    pub(crate) err: T,
}

impl<T> PositionalError<T> {
    pub(crate) fn with_fname(self, fname: String) -> ParseFileError<T> {
        ParseFileError { fname, err: self }
    }
}

/// Error that happened during parsing 'CMSIS-SVD'
#[derive(Error, Debug)]
#[cfg_attr(test, derive(PartialEq))]
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

impl From<PositionalError<CommonParseError>> for PositionalError<SvdParseError> {
    fn from(value: PositionalError<CommonParseError>) -> Self {
        let pos = value.pos;
        let (elem_name, tag) = match value.err {
            CommonParseError::ExpectedTagInElement { elem_name, tag } => (elem_name, tag),
            _other => {
                // FIXME
                panic!("");
            }
        };
        let err = SvdParseError::ExpectedTagInElement { elem_name, tag };
        PositionalError { pos, err }
    }
}

/// Error that happened during parsing 'IP-XACT'
#[derive(Error, Debug)]
pub enum IpxactParseError {
    #[error("expected tag {tag:?} in element {elem_name:?}")]
    ExpectedTagInElement { elem_name: String, tag: String },
    #[error("internal error: infallible error should not be constructible")]
    Infallible(#[from] convert::Infallible),
    #[error("could not convert from int: {0}")]
    TryFromInt(#[from] std::num::TryFromIntError),
    #[error("could not parse int: {0}")]
    ParseInt(#[from] std::num::ParseIntError),
    #[error("invalid size multiplier suffix: {0}")]
    InvalidSizeMultiplierSuffix(char),
    #[error("could not parse nonneg int from {0}")]
    InvalidNonnegInt(String),
    #[error("generic parse error")]
    GenericParse(#[from] CommonParseError),
}

impl IpxactParseError {
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

impl From<PositionalError<CommonParseError>> for PositionalError<IpxactParseError> {
    fn from(value: PositionalError<CommonParseError>) -> Self {
        let pos = value.pos;
        let (elem_name, tag) = match value.err {
            CommonParseError::ExpectedTagInElement { elem_name, tag } => (elem_name, tag),
            _other => {
                // FIXME
                panic!("");
            }
        };
        let err = IpxactParseError::ExpectedTagInElement { elem_name, tag };
        PositionalError { pos, err }
    }
}

#[derive(Error, Debug)]
#[cfg_attr(test, derive(PartialEq))]
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
        GenerateError::AddrOverflow {
            err: Box::new(value),
            archi_bits: P::ptr_size().bits(),
        }
    }
}
