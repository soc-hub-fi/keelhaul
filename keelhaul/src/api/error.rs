use std::path;

use crate::SourceFormat;
use thiserror::Error;

#[derive(Error, Debug, Clone)]
pub enum ApiError {
    #[error("not implemented")]
    NotImplemented(#[from] NotImplementedError),
    // TODO: this is a temporary error wrapper. Factor out the variants.
    #[error("keelhaul returned failure")]
    Keelhaul(#[from] crate::error::Error),
}

#[derive(Error, Debug, Clone)]
pub enum NotImplementedError {
    #[error("unsupported format '{1:?}' for path {0:?}")]
    UnsupportedSourceFormat(path::PathBuf, SourceFormat),
    #[error("unsupported option: {0:?}")]
    UnsupportedOption(String),
}

#[derive(Error, Debug)]
#[error("cannot parse test kind from {0}")]
pub struct ParseTestKindError(pub(crate) String);
