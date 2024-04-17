//! `Register` is the main primitive of the model generator. It represents all available metadata
//! for a given register and enables the generation of test cases.

use std::{cmp, fmt, hash, str};

use crate::{
    analysis,
    codegen::{self, bit_count_to_rust_uint_type_str},
    model::{bits_required, AddrRepr, UniquePath},
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
    reset_value: Option<ValueOnReset>,
    // TODO: consider support for array-like registers in input
    //dimensions: Option<svd::DimElement>,
}

impl Register {
    pub(crate) fn new(
        path: RegPath,
        addr: AddrRepr,
        size: u32,
        access: svd::Access,
        reset_value: Option<ValueOnReset>,
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

    fn reset_value(&self) -> Option<ValueOnReset> {
        self.reset_value
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

/// Register metadata commonly references its default value on reset, which we
/// can automatically check for correctness. CMSIS-SVD requires that reset
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
pub(crate) struct ValueOnReset {
    /// Known value on reset
    value: u64,
    /// An optional reset mask, indicating which bits have the defined reset value
    ///
    /// Required by CMSIS-SVD but not required by our generator.
    mask: Option<u64>,
}

impl ValueOnReset {
    /// # Arguments
    ///
    /// * `size` - register size, required for the assertion that the reset value fits the register
    pub(crate) fn new(value: u64, mask: Option<u64>, size: u32) -> Self {
        // Check that `size` can still represent this value
        assert!(bits_required(value) <= size);
        if let Some(mask) = mask {
            assert!(bits_required(mask) <= size);
        }

        Self { value, mask }
    }

    pub(crate) fn value(&self) -> u64 {
        self.value
    }

    pub(crate) fn mask(&self) -> Option<u64> {
        self.mask
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
    pub const fn max_value(self) -> u64 {
        match self {
            Self::U8 => u8::MAX as u64,
            Self::U16 => u16::MAX as u64,
            Self::U32 => u32::MAX as u64,
            Self::U64 => u64::MAX,
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
