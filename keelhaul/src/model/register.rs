//! `Register` is the main primitive of the model generator. It represents all available metadata
//! for a given register and enables the generation of test cases.

use std::{cmp, fmt, hash, str};

use crate::{
    analysis, bit_count_to_rust_uint_type_str, codegen, error,
    model::{AddrRepr, UniquePath},
};
use itertools::Itertools;

/// Represents a single memory-mapped I/O register.
#[derive(Clone, Debug)]
pub struct Register {
    /// Hierarchical path to this register, e.g. `PERIPH-CLUSTER-REG` in CMSIS-SVD 1.2 and prior
    ///
    /// Used for generating unique identifiers and symbol names in test cases
    path: RegPath,
    /// Address of the register
    addr: AddrRepr,
    /// Bit-width of register
    size: u32,
    /// Software access rights
    ///
    /// Used for determining what types of tests can be generated.
    access: svd::Access,
    /// Expected register value after reset based on source format
    ///
    /// Checking for the value may require special considerations in registers
    /// with read-only or write-only fields. These considerations are encoded in
    /// [ResetValue].
    reset_value: Option<ResetValue>,
    // TODO: consider support for array-like registers in input
    //dimensions: Option<svd::DimElement>,
}

impl Register {
    pub(crate) fn new(
        path: RegPath,
        addr: AddrRepr,
        size: u32,
        access: svd::Access,
        reset_value: Option<ResetValue>,
    ) -> Self {
        Self {
            path,
            addr,
            size,
            access,
            reset_value,
        }
    }

    pub(crate) fn addr(&self) -> u64 {
        self.addr.full()
    }
}

impl UniquePath for Register {
    fn path(&self) -> Vec<String> {
        self.path
            .0
            .iter()
            .map(|path| path.name.clone())
            .collect_vec()
    }
}

impl codegen::TestRegister for Register {
    /// Get the absolute memory address of the register
    ///
    /// # Panics
    ///
    /// * address overflows
    fn addr(&self) -> u64 {
        Register::addr(self)
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
        self.reset_value.map(|reset_value| {
            let value = reset_value.value().as_u64();
            let mask = reset_value.mask().as_u64();
            codegen::ValueOnReset::new(value, Some(mask))
        })
    }
}

impl analysis::AnalyzeRegister for Register {
    fn has_reset_value(&self) -> bool {
        self.reset_value.is_some()
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
}

/// Hierarchical representation of a register's path
///
/// E.g., PERIPH-CLUSTER-REG or PERIPH-REG
#[derive(Clone, Debug)]
pub struct RegPath(Vec<RegPathSegment>);

#[derive(Clone, Debug)]
pub struct RegPathSegment {
    pub(crate) name: String,
}

impl RegPath {
    pub fn new(segments: Vec<RegPathSegment>) -> Self {
        Self(segments)
    }

    /// Joins the names of the path elements to one string using a separator
    #[must_use]
    pub fn join(&self, sep: &str) -> String {
        self.0.iter().map(|seg| seg.name.clone()).join(sep)
    }
}

// SVD v1.2 only methods
impl RegPath {
    /// # Safety
    ///
    /// `from_components` can only be used for CMSIS-SVD inputs with a maximum of 3 levels of
    /// hierarchy. CMSIS-SVD v1.3 and above can have more.
    pub unsafe fn from_components(periph: String, cluster: Option<String>, reg: String) -> Self {
        let mut v = vec![];
        v.push(RegPathSegment { name: periph });
        if let Some(cl) = cluster {
            v.push(RegPathSegment { name: cl });
        }
        v.push(RegPathSegment { name: reg });
        Self::new(v)
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
    pub fn as_u64(&self) -> u64 {
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
    /// Number of bits used to represent this type
    fn bit_count() -> u32;
    fn all_ones() -> T;
    /// Determines wheter this type can be used to represent `val`
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
    pub const fn max_value(self) -> RegValue {
        match self {
            Self::U8 => RegValue::U8(u8::MAX),
            Self::U16 => RegValue::U16(u16::MAX),
            Self::U32 => RegValue::U32(u32::MAX),
            Self::U64 => RegValue::U64(u64::MAX),
        }
    }

    pub(crate) const fn count_bits(self) -> u32 {
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
        write!(f, "{}", bit_count_to_rust_uint_type_str(self.count_bits()))
    }
}
