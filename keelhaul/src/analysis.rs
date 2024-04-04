//! Provides information about the input files

use crate::model;

/// Trait for types that provide information about parsed registers
pub(crate) trait AnalyzeRegister: model::UniquePath {
    fn is_readable(&self) -> bool;
    fn has_reset_value(&self) -> bool;
}
