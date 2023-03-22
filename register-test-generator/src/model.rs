//! Encodes information about memory mapped registers. This information can be
//! used to generate test cases.
use itertools::Itertools;
use log::warn;
use std::{ops, str::FromStr};

use crate::{CommonParseError, GenerateError};

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

impl FromStr for Access {
    type Err = CommonParseError;

    /// Convert from CMSIS-SVD / IP-XACT `accessType` string
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "read-only" => Ok(Access::ReadOnly),
            "write-only" => Ok(Access::WriteOnly),
            "read-write" => Ok(Access::ReadWrite),
            "writeOnce" => Ok(Access::WriteOnce),
            "read-writeOnce" => Ok(Access::ReadWriteOnce),
            s => Err(CommonParseError::InvalidAccessType(s.to_owned())),
        }
    }
}

impl ToString for Access {
    /// Convert into CMSIS-SVD / IP-XACT `accessType` string
    fn to_string(&self) -> String {
        match self {
            Access::ReadOnly => "read-only",
            Access::WriteOnly => "write-only",
            Access::ReadWrite => "read-write",
            Access::WriteOnce => "writeOnce",
            Access::ReadWriteOnce => "read-writeOnce",
        }
        .to_string()
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

    /// Convert a bit count to [PtrWidth]
    ///
    /// Returns None if the conversion cannot be done.
    #[must_use]
    pub fn from_bit_count(bc: u64) -> Option<Self> {
        match bc {
            8 => Some(PtrWidth::U8),
            16 => Some(PtrWidth::U16),
            32 => Some(PtrWidth::U32),
            64 => Some(PtrWidth::U64),
            _bc => None,
        }
    }
}

/// Hierarchical representation of a register's path
///
/// E.g., PERIPH-CLUSTER-REG or PERIPH-REG
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

    /// Joins the path elements into one string
    pub fn join(&self, sep: &str) -> String {
        let mut v = vec![&self.periph];
        if let Some(cl) = self.cluster.as_ref() {
            v.push(cl);
        }
        v.push(&self.reg);

        v.into_iter().join(sep)
    }
}

#[derive(Clone, Debug)]
pub enum AddrRepr<T: num::CheckedAdd> {
    Comps {
        base: T,
        cluster: Option<T>,
        offset: T,
    },
    Full(T),
}

impl<T> AddrRepr<T>
where
    T: num::CheckedAdd + Clone,
{
    /// Get register's absolute memory address
    ///
    /// Returns None on address overflow.
    pub fn full(&self) -> Option<T> {
        match self {
            AddrRepr::Full(addr) => Some(addr.clone()),
            AddrRepr::Comps {
                base,
                cluster,
                offset,
            } => {
                let mut addr = Some(base.clone());
                if let Some(cl) = cluster {
                    addr = addr.and_then(|x| x.checked_add(cl));
                }
                addr.and_then(|x| x.checked_add(offset))
            }
        }
    }
}

// Allow conversion from a 32-bit address representation to a 64-bit
// representation to simplify debug implementations
impl From<AddrRepr<u32>> for AddrRepr<u64> {
    fn from(value: AddrRepr<u32>) -> Self {
        match value {
            AddrRepr::Comps {
                base,
                cluster,
                offset,
            } => AddrRepr::Comps {
                base: base.into(),
                cluster: cluster.map(|x| x.into()),
                offset: offset.into(),
            },
            AddrRepr::Full(u) => AddrRepr::Full(u.into()),
        }
    }
}

// 64-bit address can be fallibly converted to a 32-bit address. Returns Err on
// overflow.
impl TryFrom<AddrRepr<u64>> for AddrRepr<u32> {
    type Error = <u64 as TryInto<u32>>::Error;

    fn try_from(value: AddrRepr<u64>) -> Result<Self, Self::Error> {
        match value {
            AddrRepr::Comps {
                base,
                cluster,
                offset,
            } => {
                let base: u32 = base.try_into()?;
                let cluster: Option<u32> = cluster.map(|x| x.try_into()).transpose()?;
                let offset: u32 = offset.try_into()?;
                Ok(AddrRepr::Comps {
                    base,
                    cluster,
                    offset,
                })
            }
            AddrRepr::Full(u) => Ok(AddrRepr::Full(u.try_into()?)),
        }
    }
}

/// Represents a single memory-mapped I/O register.
pub struct Register<T: num::CheckedAdd> {
    /// Hierarchical path, e.g. `PERIPH-CLUSTER-REG`
    pub path: RegPath,
    /// Physical address of the register
    pub addr: AddrRepr<T>,
    /// Optional reset value
    ///
    /// This is the value the register will have after device reset
    pub reset_val: Option<u64>,
    /// Read-write or read-only etc.
    pub access: Access,
    /// The size or width of this register, e.g. 8-, 16-, 32-, or 64-bit
    pub size: PtrWidth,
}

impl<T> Register<T>
where
    T: num::CheckedAdd + Clone,
    AddrRepr<u64>: From<AddrRepr<T>>,
{
    /// Get register's absolute memory address
    pub fn full_addr(&self) -> Result<T, GenerateError> {
        self.addr.full().ok_or(GenerateError::AddrOverflow(
            self.path.join("-"),
            self.addr.clone().into(),
        ))
    }

    /// Get register's unique identifier
    ///
    /// Constructed from the hierarchical path, e.g., PERIPH-CLUSTER-REG.
    pub fn uid(&self) -> String {
        self.path.join("-")
    }
}

/// A list of registers parsed from SVD or IP-XACT (newtype).
pub struct Registers<T: num::CheckedAdd>(Vec<Register<T>>);

impl<T: num::CheckedAdd> From<Vec<Register<T>>> for Registers<T> {
    fn from(value: Vec<Register<T>>) -> Self {
        Self(value)
    }
}

impl<T: num::CheckedAdd> ops::Deref for Registers<T> {
    type Target = Vec<Register<T>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
