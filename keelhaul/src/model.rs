//! Encodes information about memory mapped registers. This information can be
//! used to generate test cases.

mod addr;
mod register;

// Anything that's part of the public API of register is also part of the public API of model
pub use register::*;

// Anything that's part of the internal API of register is also part of the internal API of model
pub(crate) use addr::*;

use std::{fmt, ops};

/// Trait for types that have a unique path within a given system
///
/// This could be the "peripheral-cluster-register" chain on CMSIS-SVD or the bus traverse path on
/// IP-XACT.
pub trait UniquePath {
    /// The path of the register used for human readable identification of the register as part of a
    /// larger design. Might comprise components such as "peripheral, register cluster, register
    /// name".
    fn path(&self) -> Vec<String>;

    /// Name of the top-level element containing this register, usually the peripheral or subsystem
    /// depending on system architecture
    fn top_container_name(&self) -> String {
        self.path().first().unwrap().to_owned()
    }
}

/// Reference schema
///
/// Trait for signaling a reference schema. A type implementing `Schema` can be used to indicate
/// that another type is based on a particular schema which may impact e.g., how messages are
/// formatted.
///
/// For instance, a register path implementing `RefSchemaSvd` might talk about `clusters` while a
/// register path implementing `RefSchemaIeee1685_2014` might talk about "register blocks".
///
/// # N.b. for implementors
///
/// Note that the reference schema should not impact the model implementation, only the user facing
/// output, generated variable names, etc.
pub trait RefSchema: fmt::Debug + Clone + Copy {}

/// Marker type indicating that the marked type references CMSIS-SVD v1.2
#[derive(Debug, Clone, Copy)]
pub struct RefSchemaSvdV1_2;
impl RefSchema for RefSchemaSvdV1_2 {}

/// Marker type indicating that the marked type references CMSIS-SVD v1.3
#[derive(Debug, Clone, Copy)]
pub struct RefSchemaSvdV1_3;
impl RefSchema for RefSchemaSvdV1_3 {}

/// A list of registers parsed from SVD or IP-XACT (newtype)
///
/// # Type arguments
///
/// * `P` - type representing the architecture pointer size
pub struct Registers<S: RefSchema>(Vec<Register<S>>);

impl<S: RefSchema> From<Vec<Register<S>>> for Registers<S> {
    fn from(value: Vec<Register<S>>) -> Self {
        Self(value)
    }
}

impl<S: RefSchema> ops::Deref for Registers<S> {
    type Target = Vec<Register<S>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
