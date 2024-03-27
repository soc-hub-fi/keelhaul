//! Provides information about the input files

use crate::model;

/// Trait for types that provide information about parsed registers
pub(crate) trait AnalyzeRegister {}

impl<P: num::CheckedAdd, S: model::RefSchema> AnalyzeRegister for model::Register<P, S> {}
