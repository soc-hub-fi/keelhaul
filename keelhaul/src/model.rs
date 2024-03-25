//! Encodes information about memory mapped registers. This information can be
//! used to generate test cases.

mod register;

// Anything that's part of the public API of register is also part of the public API of model
pub use register::*;

use std::{fmt, ops};

use self::schema::svd;

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
pub trait RefSchema: fmt::Debug + Clone + Copy {
    type RegPathSegmentSource;
}

/// Marker type indicating that the marked type references CMSIS-SVD v1.2
#[derive(Debug, Clone, Copy)]
pub struct RefSchemaSvdV1_2;
impl RefSchema for RefSchemaSvdV1_2 {
    type RegPathSegmentSource = svd::HierarchyLevel;
}

/// Marker type indicating that the marked type references CMSIS-SVD v1.3
#[derive(Debug, Clone, Copy)]
pub struct RefSchemaSvdV1_3;
impl RefSchema for RefSchemaSvdV1_3 {
    type RegPathSegmentSource = svd::HierarchyLevel;
}

/// A list of registers parsed from SVD or IP-XACT (newtype)
///
/// # Type arguments
///
/// * `P` - type representing the architecture pointer size
pub struct Registers<P: num::CheckedAdd, S: RefSchema>(Vec<Register<P, S>>);

impl<P: num::CheckedAdd, S: RefSchema> From<Vec<Register<P, S>>> for Registers<P, S> {
    fn from(value: Vec<Register<P, S>>) -> Self {
        Self(value)
    }
}

impl<P: num::CheckedAdd, S: RefSchema> ops::Deref for Registers<P, S> {
    type Target = Vec<Register<P, S>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

pub(crate) mod schema {
    pub(crate) mod svd {
        pub enum HierarchyLevel {
            /// Peripheral
            Periph,
            /// Cluster
            Cluster,
            /// Register
            Reg,
        }
    }
}
