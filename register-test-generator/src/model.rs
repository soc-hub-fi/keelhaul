//! Encodes information about memory mapped registers. This information can be
//! used to generate test cases.

use crate::{
    parse_ipxact::{AddrReprIpxact, RegPathIpxact},
    parse_svd::{AddrReprSvd, RegPathSvd},
    AddrOverflowError, CommonParseError, SvdParseError,
};
use core::fmt;
use log::warn;
use std::{hash, ops, str};
use thiserror::Error;

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

impl str::FromStr for Access {
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

pub trait ArchiPtr:
    Clone +
    Eq +
    hash::Hash +
    fmt::Debug +
    Send +
    Sync +
    // Allow converting into hexadecimal representation
    fmt::LowerHex +
    // num::Num for from_str_radix
    num::Num +
    // num::CheckedAdd for pointer arithmetic / composable pointers
    num::CheckedAdd +
    // str::FromStr for converting strings into values
    str::FromStr +
    // Allow creating new values from 64-bit integers at runtime (if they fit)
    TryFrom<u64> {
    fn ptr_size() -> PtrSize;
}

impl ArchiPtr for u32 {
    fn ptr_size() -> PtrSize {
        PtrSize::U32
    }
}

impl ArchiPtr for u64 {
    fn ptr_size() -> PtrSize {
        PtrSize::U64
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum PtrSize {
    U8,
    U16,
    U32,
    U64,
}

impl PtrSize {
    /// E.g., u8, u16, u32, u64
    pub fn to_rust_type_str(&self) -> &str {
        match self {
            PtrSize::U8 => "u8",
            PtrSize::U16 => "u16",
            PtrSize::U32 => "u32",
            PtrSize::U64 => "u64",
        }
    }

    /// Convert a bit count to [PtrWidth]
    ///
    /// Returns None if the conversion cannot be done.
    #[must_use]
    pub fn from_bit_count(bc: u64) -> Option<Self> {
        match bc {
            8 => Some(PtrSize::U8),
            16 => Some(PtrSize::U16),
            32 => Some(PtrSize::U32),
            64 => Some(PtrSize::U64),
            _bc => None,
        }
    }

    /// Maximum value representable by a binding of type [PtrWidth]
    pub(crate) fn max_value(&self) -> RegValue {
        match self {
            PtrSize::U8 => RegValue::U8(u8::MAX),
            PtrSize::U16 => RegValue::U16(u16::MAX),
            PtrSize::U32 => RegValue::U32(u32::MAX),
            PtrSize::U64 => RegValue::U64(u64::MAX),
        }
    }

    pub(crate) fn zero_value(&self) -> RegValue {
        match self {
            PtrSize::U8 => RegValue::U8(0),
            PtrSize::U16 => RegValue::U16(0),
            PtrSize::U32 => RegValue::U32(0),
            PtrSize::U64 => RegValue::U64(0),
        }
    }

    pub(crate) fn bits(&self) -> u8 {
        match self {
            PtrSize::U8 => 8,
            PtrSize::U16 => 16,
            PtrSize::U32 => 32,
            PtrSize::U64 => 64,
        }
    }
}

impl fmt::Display for PtrSize {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_rust_type_str())
    }
}

/// Hierarchical representation of a register's path
pub enum RegPath {
    Svd(RegPathSvd),
    Ipxact(RegPathIpxact),
}

impl RegPath {
    /// Joins the path elements into one string
    pub fn join(&self, sep: &str) -> String {
        match self {
            Self::Svd(p) => p.join(sep),
            Self::Ipxact(p) => p.join(sep),
        }
    }

    pub fn reg(&self) -> String {
        match self {
            Self::Svd(svd) => svd.reg.clone(),
            Self::Ipxact(ipxact) => ipxact.register.clone(),
        }
    }

    pub fn periph(&self) -> String {
        match self {
            Self::Svd(svd) => svd.periph.clone(),
            Self::Ipxact(ipxact) => ipxact.periph(),
        }
    }
}

/// Address representation
///
/// Addresses can be represented as full addresses, such as 0xdead_beef or as
/// components in SVD or IP-XACT, e.g., base + cluster offset + offset. This
/// type allows converting between the two.
///
/// # Type arguments
///
/// * `P` - type representing the architecture pointer size
#[derive(Clone, Debug, PartialEq)]
pub enum AddrRepr<P: num::CheckedAdd> {
    Svd(AddrReprSvd<P>),
    Ipxact(AddrReprIpxact<P>),
}

impl<P> AddrRepr<P> where P: num::CheckedAdd {}

impl<P: num::CheckedAdd + fmt::LowerHex> fmt::Display for AddrRepr<P> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Svd(svd) => svd.fmt(f),
            Self::Ipxact(ipxact) => ipxact.fmt(f),
        }
    }
}

/// Represents a single memory-mapped I/O register.
///
/// # Type arguments
///
/// * `P` - type representing the architecture pointer size
pub struct Register<P: num::CheckedAdd> {
    /// Hierarchical path, e.g. `PERIPH-CLUSTER-REG`
    pub path: RegPath,
    /// Physical address of the register
    pub addr: AddrRepr<P>,
    /// Defines register bit width, security and reset properties.
    ///
    /// Cascades from higher levels to register level.
    pub properties: RegisterPropertiesGroup,
    pub dimensions: Option<RegisterDimElementGroup>,
}

impl<P> Register<P>
where
    P: num::CheckedAdd + Clone,
{
    /// Get register's absolute memory address
    pub fn full_addr(&self) -> Result<P, AddrOverflowError<P>> {
        match &self.addr {
            AddrRepr::Svd(svd) => svd.full(),
            AddrRepr::Ipxact(ipxact) => ipxact.full(),
        }
        .ok_or(AddrOverflowError::new(
            self.path.join("-"),
            self.addr.clone(),
        ))
    }

    /// Get register's unique identifier
    ///
    /// Constructed from the hierarchical path, e.g., PERIPH-CLUSTER-REG.
    pub fn uid(&self) -> String {
        self.path.join("-")
    }

    pub(crate) fn masked_reset(&self) -> &ResetValue {
        &self.properties.reset_value
    }
}

/// A list of registers parsed from SVD or IP-XACT (newtype)
///
/// # Type arguments
///
/// * `P` - type representing the architecture pointer size
pub struct Registers<P: num::CheckedAdd>(pub Vec<Register<P>>);

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

impl str::FromStr for Protection {
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

/// Variable-length register value
#[derive(Clone, PartialEq)]
pub(crate) enum RegValue {
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
}

impl RegValue {
    pub(crate) fn width(&self) -> PtrSize {
        match self {
            RegValue::U8(_) => PtrSize::U8,
            RegValue::U16(_) => PtrSize::U16,
            RegValue::U32(_) => PtrSize::U32,
            RegValue::U64(_) => PtrSize::U64,
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

impl From<u64> for RegValue {
    fn from(value: u64) -> Self {
        RegValue::U64(value)
    }
}

impl From<u32> for RegValue {
    fn from(value: u32) -> Self {
        RegValue::U32(value)
    }
}

impl From<u16> for RegValue {
    fn from(value: u16) -> Self {
        RegValue::U16(value)
    }
}

impl From<u8> for RegValue {
    fn from(value: u8) -> Self {
        RegValue::U8(value)
    }
}

impl From<RegValue> for u64 {
    fn from(value: RegValue) -> Self {
        match value {
            RegValue::U8(v) => v as u64,
            RegValue::U16(v) => v as u64,
            RegValue::U32(v) => v as u64,
            RegValue::U64(v) => v,
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
#[cfg_attr(test, derive(PartialEq))]
#[error("types are incompatible: {0} != {1}")]
pub struct IncompatibleTypesError(PtrSize, PtrSize);

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
            ResetValue::U8 { val, .. } => RegValue::U8(*val),
            ResetValue::U16 { val, .. } => RegValue::U16(*val),
            ResetValue::U32 { val, .. } => RegValue::U32(*val),
            ResetValue::U64 { val, .. } => RegValue::U64(*val),
        }
    }

    pub(crate) fn mask(&self) -> RegValue {
        match self {
            ResetValue::U8 { mask, .. } => RegValue::U8(*mask),
            ResetValue::U16 { mask, .. } => RegValue::U16(*mask),
            ResetValue::U32 { mask, .. } => RegValue::U32(*mask),
            ResetValue::U64 { mask, .. } => RegValue::U64(*mask),
        }
    }
}

#[derive(Clone)]
pub struct RegisterPropertiesGroup {
    /// Register value bit-width.
    pub value_size: PtrSize,
    /// Register access rights.
    pub access: Access,
    /// Register access privileges.
    pub protection: Protection,
    /// Expected register value after reset based on source format
    ///
    /// Checking for the value may require special considerations in registers
    /// with read-only or write-only fields. These considerations are encoded in
    /// [ResetValue].
    reset_value: ResetValue,
}

impl RegisterPropertiesGroup {
    pub(crate) fn new(
        value_size: PtrSize,
        access: Access,
        protection: Protection,
        reset_value: ResetValue,
    ) -> Self {
        Self {
            value_size,
            access,
            protection,
            reset_value,
        }
    }

    pub(crate) fn reset(&self) -> &ResetValue {
        &self.reset_value
    }
}

#[derive(Clone)]
pub struct RegisterDimElementGroup {
    pub dim: u64,
    pub dim_increment: u64,
    //pub dim_index: Option<>,
    //pub dim_name: Option<String>,
    //pub dim_array_index: Option<>,
}
