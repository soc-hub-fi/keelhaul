//! `Register` is the main primitive of the model generator. It represents all available metadata
//! for a given register and enables the generation of test cases.

use std::{cmp, fmt, hash, marker::PhantomData, str};

use crate::{
    bit_count_to_rust_uint_type_str, codegen, error,
    model::{RefSchema, RefSchemaSvdV1_2},
};
use itertools::Itertools;

/// Represents a single memory-mapped I/O register.
///
/// # Type arguments
///
/// * `P` - type representing the architecture pointer size
/// * `S` - marker type indicating the schema this register was constructed from (IP-XACT or SVD)
#[derive(Clone, Debug)]
pub struct Register<P: num::CheckedAdd, S: RefSchema> {
    /// Hierarchical path to this register, e.g. `PERIPH-CLUSTER-REG` in CMSIS-SVD 1.2 and prior
    ///
    /// Used for generating unique identifiers and symbol names in test cases
    pub path: RegPath<S>,
    /// Physical address of the register
    pub addr: AddrRepr<P, S>,
    /// Bit-width of register
    pub size: u32,
    /// Software access rights
    ///
    /// Used for determining what types of tests can be generated.
    pub access: svd::Access,
    /// Security privilege required to access register
    pub protection: Option<svd::Protection>,
    /// Expected register value after reset based on source format
    ///
    /// Checking for the value may require special considerations in registers
    /// with read-only or write-only fields. These considerations are encoded in
    /// [ResetValue].
    pub(crate) reset_value: ResetValue,
    pub dimensions: Option<svd::DimElement>,
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

    /// Whether this register can be written to at least once after reset
    ///
    /// Returns `false` when write operations past the first one have an undefined effect.
    pub fn is_writable_once(&self) -> bool {
        use svd::Access::*;
        match self.access {
            WriteOnly | ReadWrite | WriteOnce | ReadWriteOnce => true,
            ReadOnly => false,
        }
    }

    /// Whether this register can be written to at any time
    ///
    /// Returns `false` when write operations have an undefined effect.
    pub fn is_writable_always(&self) -> bool {
        use svd::Access::*;
        match self.access {
            WriteOnly | ReadWrite => true,
            ReadOnly | WriteOnce | ReadWriteOnce => false,
        }
    }
}

impl<P, S> codegen::TestRegister<P> for Register<P, S>
where
    P: num::CheckedAdd + Copy + fmt::Debug,
    S: RefSchema,
{
    fn path(&self) -> Vec<String> {
        self.path
            .0
            .iter()
            .map(|path| path.name.clone())
            .collect_vec()
    }

    fn addr(&self) -> P {
        self.full_addr()
            .unwrap_or_else(|_| panic!("could not resolve full address for register: {:?}", &self))
    }

    fn size(&self) -> u32 {
        self.size
    }

    /// Whether this register is software readable or not
    ///
    /// Returns `false` when read operations have an undefined effect.
    fn is_readable(&self) -> bool {
        use svd::Access::*;
        match self.access {
            ReadOnly | ReadWrite => true,
            ReadWriteOnce | WriteOnly | WriteOnce => false,
        }
    }

    fn reset_value(&self) -> Option<crate::ValueOnReset<u64>> {
        let value = self.reset_value.value().as_u64();
        let mask = self.reset_value.mask().as_u64();
        Some(codegen::ValueOnReset::new(value, Some(mask)))
    }
}

/// Hierarchical representation of a register's path
///
/// E.g., PERIPH-CLUSTER-REG or PERIPH-REG
#[derive(Clone, Debug)]
pub struct RegPath<S: RefSchema>(Vec<RegPathSegment>, PhantomData<S>);

#[derive(Clone, Debug)]
pub struct RegPathSegment {
    pub(crate) name: String,
}

impl<S: RefSchema> RegPath<S> {
    pub fn new(segments: Vec<RegPathSegment>) -> Self {
        Self(segments, PhantomData)
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
        v.push(RegPathSegment { name: periph });
        if let Some(cl) = cluster {
            v.push(RegPathSegment { name: cl });
        }
        v.push(RegPathSegment { name: reg });
        Self::new(v)
    }

    pub fn periph(&self) -> &RegPathSegment {
        unsafe { self.0.get_unchecked(0) }
    }

    pub fn reg(&self) -> &RegPathSegment {
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
        assert!(!v.is_empty(), "address must have at least base address");
        Self(v, PhantomData)
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
#[derive(Clone, Copy, Debug)]
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
    pub(crate) fn with_value_and_size(
        val: u64,
        size: PtrSize,
    ) -> Result<Self, std::num::TryFromIntError> {
        Ok(match size {
            PtrSize::U8 => RegValue::U8(val.try_into()?),
            PtrSize::U16 => RegValue::U16(val.try_into()?),
            PtrSize::U32 => RegValue::U32(val.try_into()?),
            PtrSize::U64 => RegValue::U64(val),
        })
    }

    pub(crate) const fn width(&self) -> PtrSize {
        match self {
            Self::U8(_) => PtrSize::U8,
            Self::U16(_) => PtrSize::U16,
            Self::U32(_) => PtrSize::U32,
            Self::U64(_) => PtrSize::U64,
        }
    }

    /// Bit-extend the value to 64-bits
    pub(crate) fn as_u64(&self) -> u64 {
        match self {
            RegValue::U8(u) => *u as u64,
            RegValue::U16(u) => *u as u64,
            RegValue::U32(u) => *u as u64,
            RegValue::U64(u) => *u,
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

/// `BitSized` types have a knowable size
pub(crate) trait BitSized<T>: fmt::Debug + Copy {
    fn bit_count() -> u32;
    fn all_ones() -> T;
    fn can_represent<U: cmp::PartialOrd<T>>(val: U) -> bool {
        val <= Self::all_ones()
    }
}

macro_rules! impl_bit_sized {
    ($ty:ty, $numbits:expr) => {
        impl BitSized<$ty> for $ty {
            fn bit_count() -> u32 {
                $numbits
            }
            fn all_ones() -> $ty {
                <$ty>::MAX
            }
        }
    };
}

impl_bit_sized!(u8, 8);
impl_bit_sized!(u16, 16);
impl_bit_sized!(u32, 32);
impl_bit_sized!(u64, 64);

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
    /// Convert a bit count to [`PtrSize`]
    ///
    /// Returns None if the conversion cannot be done.
    #[must_use]
    pub const fn from_bit_count(bc: u32) -> Option<Self> {
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

    pub(crate) const fn bit_count(self) -> u32 {
        match self {
            Self::U8 => 8,
            Self::U16 => 16,
            Self::U32 => 32,
            Self::U64 => 64,
        }
    }
}

pub(crate) fn is_valid_bit_count(bit_count: u32) -> bool {
    matches!(bit_count, 8 | 16 | 32 | 64)
}

impl fmt::Display for PtrSize {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", bit_count_to_rust_uint_type_str(self.bit_count()))
    }
}
