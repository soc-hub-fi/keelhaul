//! Encodes information about memory mapped registers. This information can be
//! used to generate test cases.

mod register;

// Anything that's part of the public API of register is also part of the public API of model
pub use register::*;

use std::ops;

/// A list of registers parsed from SVD or IP-XACT (newtype)
///
/// # Type arguments
///
/// * `P` - type representing the architecture pointer size
pub struct Registers<P: num::CheckedAdd>(Vec<Register<P>>);

impl<P: num::CheckedAdd> From<Vec<Register<P>>> for Registers<P> {
    fn from(value: Vec<Register<P>>) -> Self {
        Self(value)
    }
}

impl<P: num::CheckedAdd> ops::Deref for Registers<P> {
    type Target = Vec<Register<P>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
