//! Encodes information about memory mapped registers. This information can be
//! used to generate test cases.
use core::fmt;
use itertools::Itertools;
use log::warn;
use std::{hash, ops, str};
use thiserror::Error;

use crate::{AddrOverflowError, CommonParseError, SvdParseError};

/// Software access rights e.g., read-only or read-write, as defined by
/// CMSIS-SVD `accessType`.
#[derive(Clone, Copy)]
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
    #[must_use]
    pub fn is_read(&self) -> bool {
        match self {
            Self::ReadOnly | Self::ReadWrite => true,
            Self::WriteOnly => false,
            Self::WriteOnce => {
                warn!("a field uses write-once, assuming not readable");
                false
            }
            Self::ReadWriteOnce => {
                warn!("a field uses read-write-once, assuming readable");
                true
            }
        }
    }

    /// Whether this register is software writable or not
    #[must_use]
    pub const fn is_write(&self) -> bool {
        match self {
            Self::ReadOnly => false,
            Self::WriteOnly | Self::ReadWrite | Self::WriteOnce | Self::ReadWriteOnce => true,
        }
    }
}

impl str::FromStr for Access {
    type Err = CommonParseError;

    /// Convert from CMSIS-SVD / IP-XACT `accessType` string
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "read-only" => Ok(Self::ReadOnly),
            "write-only" => Ok(Self::WriteOnly),
            "read-write" => Ok(Self::ReadWrite),
            "writeOnce" => Ok(Self::WriteOnce),
            "read-writeOnce" => Ok(Self::ReadWriteOnce),
            s => Err(CommonParseError::InvalidAccessType(s.to_owned())),
        }
    }
}

impl ToString for Access {
    /// Convert into CMSIS-SVD / IP-XACT `accessType` string
    fn to_string(&self) -> String {
        match self {
            Self::ReadOnly => "read-only",
            Self::WriteOnly => "write-only",
            Self::ReadWrite => "read-write",
            Self::WriteOnce => "writeOnce",
            Self::ReadWriteOnce => "read-writeOnce",
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PtrSize {
    U8,
    U16,
    U32,
    U64,
}

impl PtrSize {
    /// E.g., u8, u16, u32, u64
    #[must_use]
    pub const fn to_rust_type_str(&self) -> &str {
        match self {
            Self::U8 => "u8",
            Self::U16 => "u16",
            Self::U32 => "u32",
            Self::U64 => "u64",
        }
    }

    /// Convert a bit count to [`PtrSize`]
    ///
    /// Returns None if the conversion cannot be done.
    #[must_use]
    pub const fn from_bit_count(bc: u64) -> Option<Self> {
        match bc {
            8 => Some(Self::U8),
            16 => Some(Self::U16),
            32 => Some(Self::U32),
            64 => Some(Self::U64),
            _bc => None,
        }
    }

    /// Maximum value representable by a binding of type [`PtrSize`]
    pub(crate) const fn max_value(self) -> RegValue {
        match self {
            Self::U8 => RegValue::U8(u8::MAX),
            Self::U16 => RegValue::U16(u16::MAX),
            Self::U32 => RegValue::U32(u32::MAX),
            Self::U64 => RegValue::U64(u64::MAX),
        }
    }

    pub(crate) const fn zero_value(self) -> RegValue {
        match self {
            Self::U8 => RegValue::U8(0),
            Self::U16 => RegValue::U16(0),
            Self::U32 => RegValue::U32(0),
            Self::U64 => RegValue::U64(0),
        }
    }

    pub(crate) const fn bits(self) -> u8 {
        match self {
            Self::U8 => 8,
            Self::U16 => 16,
            Self::U32 => 32,
            Self::U64 => 64,
        }
    }
}

impl fmt::Display for PtrSize {
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
    #[must_use]
    pub const fn from_components(periph: String, cluster: Option<String>, reg: String) -> Self {
        Self {
            periph,
            cluster,
            reg,
        }
    }

    /// Joins the path elements into one string
    #[must_use]
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
/// Addresses can be represented as full addresses, such as `0xdead_beef` or as
/// components in SVD or IP-XACT, e.g., base + cluster offset + offset. This
/// type allows converting between the two.
///
/// # Type arguments
///
/// * `P` - type representing the architecture pointer size
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct AddrRepr<P: num::CheckedAdd> {
    base: P,
    cluster: Option<P>,
    offset: P,
}

impl<P> AddrRepr<P>
where
    P: num::CheckedAdd + Clone,
{
    pub const fn new(base: P, cluster: Option<P>, offset: P) -> Self {
        Self {
            base,
            cluster,
            offset,
        }
    }

    /// Get register's absolute memory address
    ///
    /// Returns None on address overflow.
    pub fn full(&self) -> Option<P> {
        let Self {
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

impl<P: num::CheckedAdd + fmt::LowerHex> fmt::Display for AddrRepr<P> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.cluster {
            Some(cluster) => write!(
                f,
                "{{ base: {:#x}, cluster: {:#x}, offset: {:#x} }}",
                self.base, cluster, self.offset
            ),
            None => write!(
                f,
                "{{ base: {:#x}, offset: {:#x} }}",
                self.base, self.offset
            ),
        }
    }
}

// Allow conversion from a 32-bit address representation to a 64-bit
// representation to simplify debug implementations
impl From<AddrRepr<u32>> for AddrRepr<u64> {
    fn from(value: AddrRepr<u32>) -> Self {
        Self {
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
        Ok(Self {
            base,
            cluster,
            offset,
        })
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
    ///
    /// # Errors
    ///
    /// Address overflows
    pub fn full_addr(&self) -> Result<P, AddrOverflowError<P>> {
        self.addr
            .full()
            .ok_or_else(|| AddrOverflowError::new(self.path.join("-"), self.addr.clone()))
    }

    /// Get register's unique identifier
    ///
    /// Constructed from the hierarchical path, e.g., PERIPH-CLUSTER-REG.
    pub fn uid(&self) -> String {
        self.path.join("-")
    }

    pub(crate) const fn masked_reset(&self) -> &ResetValue {
        &self.properties.reset_value
    }
}

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

/// Specify the security privilege to access an address region
#[derive(Clone, Copy)]
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
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum RegValue {
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
}

impl RegValue {
    pub(crate) const fn width(&self) -> PtrSize {
        match self {
            Self::U8(_) => PtrSize::U8,
            Self::U16(_) => PtrSize::U16,
            Self::U32(_) => PtrSize::U32,
            Self::U64(_) => PtrSize::U64,
        }
    }
}

impl fmt::LowerHex for RegValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::U8(u) => u.fmt(f),
            Self::U16(u) => u.fmt(f),
            Self::U32(u) => u.fmt(f),
            Self::U64(u) => u.fmt(f),
        }
    }
}

impl From<u64> for RegValue {
    fn from(value: u64) -> Self {
        Self::U64(value)
    }
}

impl From<u32> for RegValue {
    fn from(value: u32) -> Self {
        Self::U32(value)
    }
}

impl From<u16> for RegValue {
    fn from(value: u16) -> Self {
        Self::U16(value)
    }
}

impl From<u8> for RegValue {
    fn from(value: u8) -> Self {
        Self::U8(value)
    }
}

impl From<RegValue> for u64 {
    fn from(value: RegValue) -> Self {
        match value {
            RegValue::U8(v) => Self::from(v),
            RegValue::U16(v) => Self::from(v),
            RegValue::U32(v) => Self::from(v),
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
#[derive(Clone, Copy)]
pub enum ResetValue {
    U8 { val: u8, mask: u8 },
    U16 { val: u16, mask: u16 },
    U32 { val: u32, mask: u32 },
    U64 { val: u64, mask: u64 },
}

#[derive(Debug, Error)]
#[cfg_attr(test, derive(PartialEq, Eq))]
#[error("types are incompatible: {0} != {1}")]
pub struct IncompatibleTypesError(PtrSize, PtrSize);

impl ResetValue {
    pub(crate) const fn with_mask(
        value: RegValue,
        mask: RegValue,
    ) -> Result<Self, IncompatibleTypesError> {
        match (value, mask) {
            (RegValue::U8(val), RegValue::U8(mask)) => Ok(Self::U8 { val, mask }),
            (RegValue::U16(val), RegValue::U16(mask)) => Ok(Self::U16 { val, mask }),
            (RegValue::U32(val), RegValue::U32(mask)) => Ok(Self::U32 { val, mask }),
            (RegValue::U64(val), RegValue::U64(mask)) => Ok(Self::U64 { val, mask }),
            (val, mask) => Err(IncompatibleTypesError(val.width(), mask.width())),
        }
    }

    pub(crate) const fn value(&self) -> RegValue {
        match self {
            Self::U8 { val, .. } => RegValue::U8(*val),
            Self::U16 { val, .. } => RegValue::U16(*val),
            Self::U32 { val, .. } => RegValue::U32(*val),
            Self::U64 { val, .. } => RegValue::U64(*val),
        }
    }

    pub(crate) const fn mask(&self) -> RegValue {
        match self {
            Self::U8 { mask, .. } => RegValue::U8(*mask),
            Self::U16 { mask, .. } => RegValue::U16(*mask),
            Self::U32 { mask, .. } => RegValue::U32(*mask),
            Self::U64 { mask, .. } => RegValue::U64(*mask),
        }
    }
}

#[derive(Clone, Copy)]
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
    pub(crate) const fn new(
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

    pub(crate) const fn reset(&self) -> &ResetValue {
        &self.reset_value
    }
}

#[derive(Clone, Copy)]
pub struct RegisterDimElementGroup {
    pub dim: u64,
    pub dim_increment: u64,
    //pub dim_index: Option<>,
    //pub dim_name: Option<String>,
    //pub dim_array_index: Option<>,
}
