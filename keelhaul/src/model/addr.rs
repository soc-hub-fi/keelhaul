use std::fmt;

use thiserror::Error;

/// Valid address representation
///
/// Addresses can be represented in many ways. This implementation provides a vector of subsequent
/// offsets to allow construction and retrieval based on hierarchical formats.
#[derive(Clone, Debug)]
pub(crate) struct AddrRepr {
    /// Consequtive offsets to comprise the full address
    offsets: Vec<u64>,
}

impl AddrRepr {
    /// Returns an error if the address cannot be represented using supplied size
    ///
    /// # Arguments
    ///
    /// * `v` - `Vec` of offsets to comprise the full address
    /// * `size` - Number of bits used to represent this address (target pointer)
    pub fn from_vec(v: Vec<u64>, size: u32) -> Result<Self, MakeAddrError> {
        // This would usually be be a library programming error
        assert!(!v.is_empty(), "address must have at least base address");

        make_addr(v.clone(), size, None)?;

        Ok(Self { offsets: v })
    }

    /// Get register's absolute memory address
    pub fn full(&self) -> u64 {
        let (base, offsets) = self.offsets.split_at(1);
        offsets
            .iter()
            .try_fold(
                // Safety: addr repr must have at least the base address element, which is checked for in `from_vec`
                unsafe { *base.get_unchecked(0) },
                |acc, offset| acc.checked_add(*offset),
            )
            // Correctness is checked upon construction, see [`Self::from_vec`] and [`make_addr`]
            .unwrap()
    }
}

#[derive(Error, Clone, Debug)]
#[cfg_attr(test, derive(PartialEq, Eq))]
#[error(
    "overflow: could not make components {src:?} into an address of size {size} (bits), id: {id:?}"
)]
pub struct MakeAddrError {
    /// Source offsets for the address
    pub(crate) src: Vec<u64>,
    /// Number of bits used to represent this address (target pointer)
    pub(crate) size: u32,
    /// Optional identifier
    pub(crate) id: Option<String>,
}

fn bits_required(val: u64) -> u32 {
    64 - val.leading_zeros()
}

#[test]
fn bits_required_works() {
    let test = bits_required;

    assert_eq!(test(u64::MAX), 64);
    assert_eq!(test(u32::MAX.into()), 32);
    assert_eq!(test(u16::MAX.into()), 16);
    assert_eq!(test(u8::MAX.into()), 8);
    assert_eq!(test(0b1), 1);
    assert_eq!(test(0b11), 2);
    assert_eq!(test(0b101), 3);
}

/// Construct an address from given offsets
///
/// Validates that the components can form an address that can be represented using the given size
/// (target pointer).
///
/// # Arguments
///
/// * `offsets` - List of offsets to use to form the address
/// * `size` - Number of bits used to represent this address (target pointer)
/// * `id`  - Optional address identifier for debug and tracing purposes
fn make_addr(offsets: Vec<u64>, size: u32, id: Option<String>) -> Result<u64, MakeAddrError> {
    let err = MakeAddrError {
        src: offsets.clone(),
        size,
        id,
    };

    // Add the components together, watching for overflows
    let addr = offsets
        .into_iter()
        .try_fold(0u64, |acc, x| acc.checked_add(x))
        .ok_or(err.clone());

    // Check that `size` can still represent this value
    if let Ok(addr) = addr {
        if bits_required(addr) > size {
            return Err(err);
        }
    }

    addr
}

// SVD v1.2 only methods
impl AddrRepr {
    pub fn from_base_cluster_offset(
        base: u64,
        cluster: Option<u64>,
        offset: u64,
        size: u32,
    ) -> Result<Self, MakeAddrError> {
        let mut v = vec![];
        v.push(base);
        if let Some(cl) = cluster {
            v.push(cl);
        }
        v.push(offset);
        Self::from_vec(v, size)
    }

    pub fn components(&self) -> (u64, Option<u64>, u64) {
        let ofs = &self.offsets;
        if ofs.len() == 2 {
            // Safety: length checked on conditional of previous line
            unsafe { (*ofs.get_unchecked(0), None, *ofs.get_unchecked(1)) }
        } else if ofs.len() == 3 {
            // Safety: length checked on conditional of previous line
            unsafe {
                (
                    *ofs.get_unchecked(0),
                    Some(*ofs.get_unchecked(1)),
                    *ofs.get_unchecked(2),
                )
            }
        } else {
            panic!("register in the CMSIS-SVD 1.2 schema must comprise of at least two elements (periph + offset)")
        }
    }

    pub fn base(&self) -> u64 {
        // Safety: `AddrRepr` must have at least base address
        unsafe { *self.offsets.get_unchecked(0) }
    }

    pub fn cluster(&self) -> Option<u64> {
        // Safety: length checked
        (self.offsets.len() == 3).then(|| unsafe { *self.offsets.get_unchecked(1) })
    }

    pub fn offset(&self) -> u64 {
        let v = &self.offsets;
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

impl fmt::Display for AddrRepr {
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
