//! `Register` is the main primitive of the model generator. It represents all available metadata
//! for a given register and enables the generation of test cases.

use std::{fmt, hash, marker::PhantomData, ops, str};

use crate::{
    error,
    model::{schema::svd, RefSchema, RefSchemaSvdV1_2},
};
use itertools::Itertools;
use log::warn;

/// Represents a single memory-mapped I/O register.
///
/// # Type arguments
///
/// * `P` - type representing the architecture pointer size
pub struct Register<P: num::CheckedAdd, S: RefSchema> {
    /// Hierarchical path to this register, e.g. `PERIPH-CLUSTER-REG` in CMSIS-SVD 1.2 and prior
    pub path: RegPath<S>,
    /// Physical address of the register
    pub addr: AddrRepr<P, S>,
    /// Defines register bit width, security and reset properties.
    ///
    /// Cascades from higher levels to register level.
    pub properties: RegisterPropertiesGroup,
    pub dimensions: Option<RegisterDimElementGroup>,
}

impl<P, S> Register<P, S>
where
    P: num::CheckedAdd + Copy,
    S: RefSchema,
{
    /// Get register's absolute memory address
    ///
    /// # Errors
    ///
    /// Address overflows
    pub fn full_addr(&self) -> Result<P, error::AddrOverflowError<P, S>> {
        self.addr
            .full()
            .ok_or_else(|| error::AddrOverflowError::new(self.path.join("-"), self.addr.clone()))
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

/// Hierarchical representation of a register's path
///
/// E.g., PERIPH-CLUSTER-REG or PERIPH-REG
pub struct RegPath<S: RefSchema>(Vec<RegPathSegment<S>>);

pub struct RegPathSegment<S: RefSchema> {
    pub(crate) name: String,

    /// Optional metadata indicating what caused this path segment to be generated
    ///
    /// For CMSIS-SVD this can be either "cluster" or "register"
    pub(crate) source: Option<S::RegPathSegmentSource>,
}

impl<S: RefSchema> RegPath<S> {
    pub fn new(segments: Vec<RegPathSegment<S>>) -> Self {
        Self(segments)
    }

    /// Joins the names of the path elements to one string using a separator
    #[must_use]
    pub fn join(&self, sep: &str) -> String {
        self.0.iter().map(|seg| seg.name.clone()).join(sep)
    }
}

// SVD v1.2 only methods
impl RegPath<RefSchemaSvdV1_2> {
    pub fn from_components(periph: String, cluster: Option<String>, reg: String) -> Self {
        let mut v = vec![];
        v.push(RegPathSegment {
            name: periph,
            source: Some(svd::HierarchyLevel::Periph),
        });
        if let Some(cl) = cluster {
            v.push(RegPathSegment {
                name: cl,
                source: Some(svd::HierarchyLevel::Cluster),
            });
        }
        v.push(RegPathSegment {
            name: reg,
            source: Some(svd::HierarchyLevel::Reg),
        });
        Self::new(v)
    }

    pub fn periph(&self) -> &RegPathSegment<RefSchemaSvdV1_2> {
        unsafe { self.0.get_unchecked(0) }
    }

    pub fn reg(&self) -> &RegPathSegment<RefSchemaSvdV1_2> {
        if self.0.len() == 2 {
            unsafe { self.0.get_unchecked(1) }
        } else if self.0.len() == 3 {
            unsafe { self.0.get_unchecked(2) }
        } else {
            panic!("register in the CMSIS-SVD 1.2 schema must comprise of at least two elements (periph + offset)")
        }
    }
}

/// Address representation
///
/// Addresses can be represented in many ways. This implementation provides a vector of subsequent
/// offsets.
///
/// # Type arguments
///
/// * `P` - type representing the architecture pointer size
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AddrRepr<P: num::CheckedAdd, S: RefSchema>(Vec<P>, PhantomData<S>);

impl<P, S> AddrRepr<P, S>
where
    P: num::CheckedAdd + Copy,
    S: RefSchema,
{
    pub fn from_vec(v: Vec<P>) -> Self {
        assert!(v.len() != 0, "address must have at least base address");
        Self(v, PhantomData::default())
    }

    /// Get register's absolute memory address
    ///
    /// Returns None on address overflow.
    pub fn full(&self) -> Option<P> {
        let (base, offsets) = self.0.split_at(1);
        offsets.iter().try_fold(
            // Safety: addr repr must have at least the base address element, which is checked for in `from_vec`
            unsafe { *base.get_unchecked(0) },
            |acc, offset| acc.checked_add(offset),
        )
    }
}

// SVD v1.2 only methods
impl<P> AddrRepr<P, RefSchemaSvdV1_2>
where
    P: num::CheckedAdd + Copy,
{
    pub fn from_base_cluster_offset(base: P, cluster: Option<P>, offset: P) -> Self {
        let mut v = vec![];
        v.push(base);
        if let Some(cl) = cluster {
            v.push(cl);
        }
        v.push(offset);
        Self::from_vec(v)
    }

    pub fn components(&self) -> (P, Option<P>, P) {
        let v = &self.0;
        if v.len() == 2 {
            // Safety: length checked on conditional of previous line
            unsafe { (*v.get_unchecked(0), None, *v.get_unchecked(1)) }
        } else if v.len() == 3 {
            // Safety: length checked on conditional of previous line
            unsafe {
                (
                    *v.get_unchecked(0),
                    Some(*v.get_unchecked(1)),
                    *v.get_unchecked(2),
                )
            }
        } else {
            panic!("register in the CMSIS-SVD 1.2 schema must comprise of at least two elements (periph + offset)")
        }
    }

    pub fn base(&self) -> P {
        // Safety: `AddrRepr` must have at least base address
        unsafe { *self.0.get_unchecked(0) }
    }

    pub fn cluster(&self) -> Option<P> {
        // Safety: length checked
        (self.0.len() == 3).then(|| unsafe { *self.0.get_unchecked(1) })
    }

    pub fn offset(&self) -> P {
        let v = &self.0;
        if v.len() == 2 {
            // Safety: length checked on conditional of previous line
            unsafe { *v.get_unchecked(1) }
        } else if v.len() == 3 {
            // Safety: length checked on conditional of previous line
            unsafe { *v.get_unchecked(2) }
        } else {
            panic!("register in the CMSIS-SVD 1.2 schema must comprise of at least two elements (periph + offset)")
        }
    }
}

impl<P: num::CheckedAdd + fmt::LowerHex + Copy> fmt::Display for AddrRepr<P, RefSchemaSvdV1_2> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.cluster() {
            Some(cluster) => write!(
                f,
                "{{ base: {:#x}, cluster: {:#x}, offset: {:#x} }}",
                self.base(),
                cluster,
                self.offset()
            ),
            None => write!(
                f,
                "{{ base: {:#x}, offset: {:#x} }}",
                self.base(),
                self.offset()
            ),
        }
    }
}

// Allow conversion from a 32-bit address representation to a 64-bit
// representation to simplify debug implementations
impl From<AddrRepr<u32, RefSchemaSvdV1_2>> for AddrRepr<u64, RefSchemaSvdV1_2> {
    fn from(value: AddrRepr<u32, RefSchemaSvdV1_2>) -> Self {
        Self::from_base_cluster_offset(
            value.base().into(),
            value.cluster().map(|x| x.into()),
            value.offset().into(),
        )
    }
}

// 64-bit address can be fallibly converted to a 32-bit address. Returns Err on
// overflow.
impl<S: RefSchema> TryFrom<AddrRepr<u64, S>> for AddrRepr<u32, S> {
    type Error = <u64 as TryInto<u32>>::Error;

    fn try_from(value: AddrRepr<u64, S>) -> Result<Self, Self::Error> {
        Ok(Self::from_vec(
            value
                .0
                .into_iter()
                .map(|v| v.try_into())
                .collect::<Result<Vec<_>, _>>()?,
        ))
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
    type Err = error::SvdParseError;

    /// Convert from CMSIS-SVD `protectionStringType` string
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "s" => Ok(Self::Secure),
            "n" => Ok(Self::NonSecureOrSecure),
            "p" => Ok(Self::Privileged),
            _ => Err(error::SvdParseError::InvalidProtectionType(s.to_owned())),
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

#[derive(Debug, Clone)]
pub struct RegisterDimElementGroup {
    pub dim: u64,
    pub dim_increment: u64,
    pub dim_index: Option<DimIndex>,
    pub dim_name: Option<String>,
    // TODO: this is not a number
    pub dim_array_index: Option<usize>,
}

#[derive(Debug, Clone)]
pub enum DimIndex {
    NumberRange(ops::RangeInclusive<usize>),
    LetterRange(ops::RangeInclusive<char>),
    List(Vec<String>),
}

impl DimIndex {
    pub fn get(&self, index: usize) -> String {
        match self {
            Self::NumberRange(range) => {
                let mut range = range.clone();
                // TODO: use error
                range.nth(index).expect("").to_string()
            }
            Self::LetterRange(range) => {
                let mut range = range.clone();
                // TODO: use error
                range.nth(index).expect("").to_string()
            }
            Self::List(list) => {
                // TODO: use error
                list.get(index).expect("").to_string()
            }
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

impl ResetValue {
    pub(crate) const fn with_mask(
        value: RegValue,
        mask: RegValue,
    ) -> Result<Self, error::IncompatibleTypesError> {
        match (value, mask) {
            (RegValue::U8(val), RegValue::U8(mask)) => Ok(Self::U8 { val, mask }),
            (RegValue::U16(val), RegValue::U16(mask)) => Ok(Self::U16 { val, mask }),
            (RegValue::U32(val), RegValue::U32(mask)) => Ok(Self::U32 { val, mask }),
            (RegValue::U64(val), RegValue::U64(mask)) => Ok(Self::U64 { val, mask }),
            (val, mask) => Err(error::IncompatibleTypesError(val.width(), mask.width())),
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
    type Err = error::CommonParseError;

    /// Convert from CMSIS-SVD / IP-XACT `accessType` string
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "read-only" => Ok(Self::ReadOnly),
            "write-only" => Ok(Self::WriteOnly),
            "read-write" => Ok(Self::ReadWrite),
            "writeOnce" => Ok(Self::WriteOnce),
            "read-writeOnce" => Ok(Self::ReadWriteOnce),
            s => Err(error::CommonParseError::InvalidAccessType(s.to_owned())),
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

pub trait ArchPtr:
    Clone + Copy +
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
    TryFrom<u64>
    {
    fn ptr_size() -> PtrSize;
}

impl ArchPtr for u8 {
    fn ptr_size() -> PtrSize {
        PtrSize::U8
    }
}

impl ArchPtr for u16 {
    fn ptr_size() -> PtrSize {
        PtrSize::U16
    }
}

impl ArchPtr for u32 {
    fn ptr_size() -> PtrSize {
        PtrSize::U32
    }
}

impl ArchPtr for u64 {
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

impl std::convert::TryFrom<u8> for PtrSize {
    type Error = error::NotImplementedError;
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            1 => Ok(Self::U8),
            2 => Ok(Self::U16),
            4 => Ok(Self::U32),
            8 => Ok(Self::U64),
            other => Err(error::NotImplementedError::PtrSize(other)),
        }
    }
}
