//! Encodes information about memory mapped registers. This information can be
//! used to generate test cases.
use log::warn;
use std::ops;

use crate::{AddrRepr, GenerateError, ParseError};

/// Software access rights e.g., read-only or read-write, as defined by
/// CMSIS-SVD `accessType`.
pub enum Access {
    /// read-only
    ReadOnly,
    /// write-only
    WriteOnly,
    /// read-write
    ReadWrite,
    /// writeOnce
    WriteOnce,
    /// read-writeOnce
    ReadWriteOnce,
}

impl Access {
    /// Whether this register is software readable or not
    pub fn is_read(&self) -> bool {
        match self {
            Access::ReadOnly | Access::ReadWrite => true,
            Access::WriteOnly => false,
            Access::WriteOnce => {
                warn!("a field uses write-once, assuming not readable");
                false
            }
            Access::ReadWriteOnce => {
                warn!("a field uses read-write-once, assuming readable");
                true
            }
        }
    }

    /// Whether this register is software writable or not
    pub fn is_write(&self) -> bool {
        match self {
            Access::ReadOnly => false,
            Access::WriteOnly | Access::ReadWrite | Access::WriteOnce | Access::ReadWriteOnce => {
                true
            }
        }
    }
}

pub enum PtrWidth {
    U8,
    U16,
    U32,
    U64,
}

impl PtrWidth {
    /// E.g., u8, u16, u32, u64
    pub fn to_rust_type_str(&self) -> &str {
        match self {
            PtrWidth::U8 => "u8",
            PtrWidth::U16 => "u16",
            PtrWidth::U32 => "u32",
            PtrWidth::U64 => "u64",
        }
    }

    pub fn from_bit_count(bc: u64) -> Result<Self, ParseError> {
        match bc {
            8 => Ok(PtrWidth::U8),
            16 => Ok(PtrWidth::U16),
            32 => Ok(PtrWidth::U32),
            64 => Ok(PtrWidth::U64),
            bc => Err(ParseError::BitCountToPtrWidth(bc)),
        }
    }
}

/// Represents a single memory-mapped I/O register.
pub struct Register {
    pub peripheral_name: String,
    pub cluster_name: String,
    pub reg_name: String,
    pub base_addr: u64,
    pub cluster_addr_offset: u64,
    pub reg_addr_offset: u64,
    // TODO: this should be optional but isn't?
    pub reset_val: u64,
    pub is_read: bool,
    pub is_write: bool,
    pub size: PtrWidth,
}

impl Register {
    /// Get register's absolute memory address.
    pub fn full_address(&self) -> Result<u64, GenerateError> {
        let base = self.base_addr;
        let cluster = self.cluster_addr_offset;
        let offset = self.reg_addr_offset;
        let err = GenerateError::AddrOverflow(
            self.full_path("-"),
            AddrRepr::Comps {
                base,
                cluster,
                offset,
            },
        );
        base.checked_add(cluster)
            .and_then(|x| x.checked_add(offset))
            .ok_or(err)
    }

    pub fn full_path(&self, sep: &str) -> String {
        [
            self.peripheral_name.clone(),
            self.cluster_name.clone(),
            self.reg_name.clone(),
        ]
        .join(sep)
    }

    /// Get register's unique identifier.
    pub fn uid(&self) -> String {
        self.full_path("-")
    }
}

/// A list of registers parsed from SVD or IP-XACT (newtype).
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
