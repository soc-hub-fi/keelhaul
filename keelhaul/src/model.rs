//! Encodes information about memory mapped registers. This information can be
//! used to generate test cases.

mod addr;
mod register;

// Anything that's part of the public API of register is also part of the public API of model
pub use register::*;

// Anything that's part of the internal API of register is also part of the internal API of model
pub(crate) use addr::*;

use std::ops;

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

/// A list of registers parsed from SVD or IP-XACT (newtype)
pub struct Registers(Vec<Register>);

impl From<Vec<Register>> for Registers {
    fn from(value: Vec<Register>) -> Self {
        Self(value)
    }
}

impl ops::Deref for Registers {
    type Target = Vec<Register>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

pub(crate) fn bits_required(val: u64) -> u32 {
    64 - val.leading_zeros()
}

#[test]
fn bits_required_works() {
    let test = bits_required;

    assert_eq!(test(u64::MAX), 64);
    assert_eq!(test(u32::MAX.into()), 32);
    assert_eq!(test(u16::MAX.into()), 16);
    assert_eq!(test(u8::MAX.into()), 8);
    assert_eq!(test(0b1), 1);
    assert_eq!(test(0b11), 2);
    assert_eq!(test(0b101), 3);
}
