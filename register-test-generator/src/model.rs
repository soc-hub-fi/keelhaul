//! Encodes information about memory mapped registers. This information can be
//! used to generate test cases.
use core::fmt;
use itertools::Itertools;
use log::warn;
use std::{ops, str::FromStr};
use thiserror::Error;

use crate::{AddrOverflowError, CommonParseError, SvdParseError};

/// Software access rights e.g., read-only or read-write, as defined by
/// CMSIS-SVD `accessType`.
#[derive(Clone)]
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

#[derive(Clone, Debug)]
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

    /// Maximum value representable by a binding of type [PtrWidth]
    pub(crate) fn max_value(&self) -> RegValue {
        match self {
            PtrWidth::U8 => RegValue::U8(u8::MAX),
            PtrWidth::U16 => RegValue::U16(u16::MAX),
            PtrWidth::U32 => RegValue::U32(u32::MAX),
            PtrWidth::U64 => RegValue::U64(u64::MAX),
        }
    }
}

impl fmt::Display for PtrWidth {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_rust_type_str())
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

/// Address representation
///
/// Addresses can be represented as full addresses, such as 0xdead_beef or as
/// components in SVD or IP-XACT, e.g., base + cluster offset + offset. This
/// type allows converting between the two.
#[derive(Clone, Debug)]
pub struct AddrRepr<T: num::CheckedAdd> {
    base: T,
    cluster: Option<T>,
    offset: T,
}

impl<T> AddrRepr<T>
where
    T: num::CheckedAdd + Clone,
{
    pub fn new(base: T, cluster: Option<T>, offset: T) -> Self {
        Self {
            base,
            cluster,
            offset,
        }
    }

    /// Get register's absolute memory address
    ///
    /// Returns None on address overflow.
    pub fn full(&self) -> Option<T> {
        let AddrRepr {
            base,
            cluster,
            offset,
        } = self;

        let mut addr = Some(base.clone());
        if let Some(cl) = cluster {
            addr = addr.and_then(|x| x.checked_add(cl));
        }
        addr.and_then(|x| x.checked_add(offset))
    }
}

// Allow conversion from a 32-bit address representation to a 64-bit
// representation to simplify debug implementations
impl From<AddrRepr<u32>> for AddrRepr<u64> {
    fn from(value: AddrRepr<u32>) -> Self {
        AddrRepr {
            base: value.base.into(),
            cluster: value.cluster.map(|x| x.into()),
            offset: value.offset.into(),
        }
    }
}

// 64-bit address can be fallibly converted to a 32-bit address. Returns Err on
// overflow.
impl TryFrom<AddrRepr<u64>> for AddrRepr<u32> {
    type Error = <u64 as TryInto<u32>>::Error;

    fn try_from(value: AddrRepr<u64>) -> Result<Self, Self::Error> {
        let AddrRepr {
            base,
            cluster,
            offset,
        } = value;

        let base: u32 = base.try_into()?;
        let cluster: Option<u32> = cluster.map(|x| x.try_into()).transpose()?;
        let offset: u32 = offset.try_into()?;
        Ok(AddrRepr {
            base,
            cluster,
            offset,
        })
    }
}

/// Represents a single memory-mapped I/O register.
pub struct Register<T: num::CheckedAdd> {
    /// Hierarchical path, e.g. `PERIPH-CLUSTER-REG`
    pub path: RegPath,
    /// Physical address of the register
    pub addr: AddrRepr<T>,
    /// Defines register bit width, security and reset properties.
    ///
    /// Cascades from higher levels to register level.
    pub properties: RegisterPropertiesGroup,
    pub dimensions: Option<RegisterDimElementGroup>,
}

impl<T> Register<T>
where
    T: num::CheckedAdd + Clone,
{
    /// Get register's absolute memory address
    pub fn full_addr(&self) -> Result<T, AddrOverflowError<T>> {
        self.addr.full().ok_or(AddrOverflowError(
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

/// Specify the security privilege to access an address region
#[derive(Clone)]
pub enum Protection {
    /// Secure permission required for access
    Secure,
    /// Non-secure or secure permission required for access
    NonSecureOrSecure,
    /// Privileged permission required for access
    Privileged,
}

impl FromStr for Protection {
    type Err = SvdParseError;

    /// Convert from CMSIS-SVD `protectionStringType` string
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "s" => Ok(Self::Secure),
            "n" => Ok(Self::NonSecureOrSecure),
            "p" => Ok(Self::Privileged),
            _ => Err(SvdParseError::InvalidProtectionType(s.to_owned())),
        }
    }
}

impl ToString for Protection {
    /// Convert to CMSIS-SVD `protectionStringType` string
    fn to_string(&self) -> String {
        match self {
            Self::Secure => "s",
            Self::NonSecureOrSecure => "n",
            Self::Privileged => "p",
        }
        .to_owned()
    }
}

#[derive(Clone)]
pub struct RegisterPropertiesGroupBuilder {
    /// Register bit-width.
    pub size: Option<PtrWidth>,
    /// Register access rights.
    pub access: Option<Access>,
    /// Register access privileges.
    pub protection: Option<Protection>,
    /// Register value after reset.
    /// Actual reset value is calculated using reset value and reset mask.
    pub reset_value: Option<u64>,
    /// Register bits with defined reset value are marked as high.
    pub reset_mask: Option<u64>,
}

/// Variable-length register value
#[derive(Clone, PartialEq)]
pub(crate) enum RegValue {
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
}

impl RegValue {
    pub(crate) fn width(&self) -> PtrWidth {
        match self {
            RegValue::U8(_) => PtrWidth::U8,
            RegValue::U16(_) => PtrWidth::U16,
            RegValue::U32(_) => PtrWidth::U32,
            RegValue::U64(_) => PtrWidth::U64,
        }
    }
}

impl fmt::LowerHex for RegValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RegValue::U8(u) => u.fmt(f),
            RegValue::U16(u) => u.fmt(f),
            RegValue::U32(u) => u.fmt(f),
            RegValue::U64(u) => u.fmt(f),
        }
    }
}

/// Variable-length register reset value
///
/// Register metadata commonly references its default value on reset, which we
/// can automatically check for correctness. CMSIS-SVD mandates that reset
/// values are always reported with their appropriate reset mask. This enables
/// test cases to avoid reading from write-only bits or writing to read-only
/// bits.
///
/// # Fields
///
/// `val`   - The reset value
/// `mask`  - Mask for reading from or writing into the register, to assist in
/// writing into partially protected registers.
#[derive(Clone)]
pub(crate) enum ResetValue {
    U8 { val: u8, mask: u8 },
    U16 { val: u16, mask: u16 },
    U32 { val: u32, mask: u32 },
    U64 { val: u64, mask: u64 },
}

#[derive(Debug, Error)]
#[error("types are incompatible: {0} != {1}")]
pub struct IncompatibleTypesError(PtrWidth, PtrWidth);

impl ResetValue {
    pub(crate) fn with_mask(
        value: RegValue,
        mask: RegValue,
    ) -> Result<Self, IncompatibleTypesError> {
        match (value, mask) {
            (RegValue::U8(val), RegValue::U8(mask)) => Ok(ResetValue::U8 { val, mask }),
            (RegValue::U16(val), RegValue::U16(mask)) => Ok(ResetValue::U16 { val, mask }),
            (RegValue::U32(val), RegValue::U32(mask)) => Ok(ResetValue::U32 { val, mask }),
            (RegValue::U64(val), RegValue::U64(mask)) => Ok(ResetValue::U64 { val, mask }),
            (val, mask) => Err(IncompatibleTypesError(val.width(), mask.width())),
        }
    }

    pub(crate) fn value(&self) -> RegValue {
        match self {
            ResetValue::U8 { val, .. } => RegValue::U8(val.clone()),
            ResetValue::U16 { val, .. } => RegValue::U16(val.clone()),
            ResetValue::U32 { val, .. } => RegValue::U32(val.clone()),
            ResetValue::U64 { val, .. } => RegValue::U64(val.clone()),
        }
    }

    pub(crate) fn mask(&self) -> RegValue {
        match self {
            ResetValue::U8 { mask, .. } => RegValue::U8(mask.clone()),
            ResetValue::U16 { mask, .. } => RegValue::U16(mask.clone()),
            ResetValue::U32 { mask, .. } => RegValue::U32(mask.clone()),
            ResetValue::U64 { mask, .. } => RegValue::U64(mask.clone()),
        }
    }
}

#[derive(Clone)]
pub struct RegisterPropertiesGroup {
    /// Register value bit-width.
    pub value_size: PtrWidth,
    /// Register access rights.
    pub access: Access,
    /// Register access privileges.
    pub protection: Protection,
    // HACK: it's not correct to use u64 for `reset_value` and `reset_mask`.
    // Instead it would be correct to use the type of whatever's contained in
    // the register (not the architecture type). However, u64 is way easier to
    // implement for now, and it's valid to cast the u64 to any smaller types
    // when necessary.
    /// Register value after reset.
    /// Actual reset value is calculated using reset value and reset mask.
    pub reset_value: u64,
    /// Register bits with defined reset value are marked as high.
    pub reset_mask: u64,
}

#[derive(Clone)]
pub struct RegisterDimElementGroup {
    pub dim: u64,
    pub dim_increment: u64,
    //pub dim_index: Option<>,
    //pub dim_name: Option<String>,
    //pub dim_array_index: Option<>,
}
