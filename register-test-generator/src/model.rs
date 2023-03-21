//! Encodes information about memory mapped registers. This information can be
//! used to generate test cases.
use itertools::Itertools;
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

/// Hierarchical representation of a register's path
///
/// E.g., PERIPHERAL-CLUSTER-REG
pub struct RegPath {
    pub periph: String,
    pub cluster: Option<String>,
    pub reg: String,
}

impl RegPath {
    pub fn from_components(periph: String, cluster: Option<String>, reg: String) -> Self {
        Self {
            periph,
            cluster,
            reg,
        }
    }

    pub fn join(&self, sep: &str) -> String {
        let mut v = vec![&self.periph];
        if let Some(cl) = self.cluster.as_ref() {
            v.push(cl);
        }
        v.push(&self.reg);

        v.into_iter().join(sep)
    }
}

/// Represents a single memory-mapped I/O register.
pub struct Register {
    pub path: RegPath,
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
            self.path.join("-"),
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

    /// Get register's unique identifier.
    pub fn uid(&self) -> String {
        self.path.join("-")
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
