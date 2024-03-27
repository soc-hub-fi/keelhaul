//! Provides information about the input files

use std::fmt;

use crate::model;

/// Trait for types that provide information about parsed registers
pub(crate) trait AnalyzeRegister: model::UniquePath {}

impl<P: num::CheckedAdd + Copy + fmt::Debug, S: model::RefSchema> AnalyzeRegister
    for model::Register<P, S>
{
}
