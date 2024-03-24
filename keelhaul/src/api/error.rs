use std::path;

use crate::SourceFormat;
use thiserror::Error;

#[derive(Error, Debug, Clone)]
pub enum ApiError {
    #[error("not implemented")]
    NotImplemented(#[from] NotImplementedError),
    // TODO: this is a temporary error wrapper. Factor out the variants.
    #[error("keelhaul internal error")]
    Keelhaul(#[from] crate::error::Error),
}

#[derive(Error, Debug, Clone)]
pub enum NotImplementedError {
    #[error("unsupported format '{1:?}' for path {0:?}")]
    UnsupportedSourceFormat(path::PathBuf, SourceFormat),
}