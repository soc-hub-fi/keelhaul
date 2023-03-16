//! Encodes information about memory mapped registers. This information can be
//! used to generate test cases.
use log::warn;
use std::ops;

use crate::{AddrRepr, GenerateError};

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

/// Represents a single memory-mapped I/O register.
pub struct Register {
    pub peripheral_name: String,
    pub cluster_name: String,
    pub reg_name: String,
    pub base_addr: u64,
    pub cluster_addr_offset: u64,
    pub reg_addr_offset: u64,
    pub reset_val: u64,
    pub is_read: bool,
    pub is_write: bool,
    pub size: u64,
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
        format!(
            "{}-{}-{}",
            self.peripheral_name, self.cluster_name, self.reg_name
        )
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
