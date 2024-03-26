//! Generate test cases from [`model::*`] types

// TODO: maybe generate array registers under new module-level

use std::{
    any, cmp,
    collections::{HashMap, HashSet},
    fmt, iter, str,
};

use crate::{
    error::GenerateError,
    model::{self, ArchPtr, Registers},
};
use itertools::Itertools;
use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote};
use strum::{EnumIter, IntoEnumIterator};
use thiserror::Error;

/// Type that a test can be generated for
pub(crate) trait TestRegister<P> {
    /// The path of the register used for human readable identification of the register as part of a
    /// larger design. Might comprise components such as "peripheral, register cluster, register
    /// name".
    fn path(&self) -> Vec<String>;

    /// A human-readable unique identifier for the register, usually the the path that is used to
    /// access the register.
    fn uid(&self) -> String {
        self.path().join("_")
    }

    /// Name of the top-level element containing this register, usually the first element of the
    /// `path`
    fn periph_name(&self) -> String {
        self.path().first().unwrap().to_owned()
    }

    /// The name of the register, usually the final element of the `path`
    fn name(&self) -> String {
        self.path().last().unwrap().to_owned()
    }

    /// The address of the register
    fn addr(&self) -> P;

    /// The size of the register in bits
    fn size(&self) -> u32;

    /// Whether reading the register has a defined effect
    fn is_readable(&self) -> bool;

    /// An optional, known reset value
    fn reset_value(&self) -> Option<ValueOnReset<u64>>;
}

#[derive(Clone, Debug)]
pub(crate) struct ValueOnReset<T> {
    /// Known value on reset
    value: T,
    /// An optional reset mask, indicating which bits have the defined reset value
    mask: Option<T>,
}

impl<T> ValueOnReset<T> {
    pub(crate) fn new(value: T, mask: Option<T>) -> Self {
        Self { value, mask }
    }
}

fn gen_preamble(arch: model::PtrSize, error_derive_debug: bool) -> TokenStream {
    // It costs a lot of code size to `#[derive(Debug)]` so we only do it if required
    let opt_derive_debug = if error_derive_debug {
        quote!(#[derive(Debug)])
    } else {
        quote!()
    };

    // Generate an error variant for all test case kinds
    let error_variant_defs: TokenStream = RegTestKind::iter()
        .filter_map(|test_kind| test_kind.error_variant_def(arch))
        .collect();

    quote! {
        use core::ptr::*;

        #opt_derive_debug
        pub enum Error {
            #error_variant_defs
        }

        pub type Result<T> = core::result::Result<T, Error>;

        /// Represents a test case for a single register.
        pub struct TestCase<'a> {
            pub function: fn() -> Result<()>,
            pub addr: usize,
            pub uid: &'a str,
        }
    }
}

/// Place test cases in modules.
fn create_modules(
    test_fns_and_defs_by_periph: HashMap<String, Vec<(String, String)>>,
) -> Vec<TokenStream> {
    test_fns_and_defs_by_periph
        .into_iter()
        .map(|(periph_name, test_fns_and_defs)| {
            // Separate test functions and definitions into two Vecs
            let (test_fns, test_defs): (Vec<_>, Vec<_>) = test_fns_and_defs.into_iter().unzip();

            // Create array for test definitions
            let test_defs_combined: TokenStream = test_defs.join(", ").parse().unwrap();
            let len = test_fns.len();
            let value: TokenStream = quote!( [#test_defs_combined] );
            let test_def_arr = quote! {
                pub static TEST_CASES: [TestCase; #len] = #value;
            };

            // Create test functions
            let mod_name: TokenStream = periph_name.to_lowercase().parse().unwrap();
            let test_fns_catenated: TokenStream = test_fns.join("").parse().unwrap();

            // Create module with both test definitions and test functions
            quote! {
                pub mod #mod_name {
                    use super::*;

                    #test_def_arr

                    #test_fns_catenated
                }
            }
        })
        .collect_vec()
}

#[derive(Error, Debug)]
#[error("cannot parse test kind from {0}")]
pub struct ParseTestKindError(String);

/// Type of metadata test generatable by keelhaul
#[derive(Clone, Debug, Hash, PartialEq, Eq, EnumIter)]
pub enum RegTestKind {
    /// The register value will be read, though nothing will be done with the output
    Read,
    /// The register value will be read and compared compared against the known reset value
    ///
    /// Reset tests are only generated for registers that have a known reset value.
    ReadIsResetVal,
}

impl RegTestKind {
    /// Error variant for an error enumeration, including the comma at end
    ///
    /// # Parameters
    ///
    /// - `arch_ptr_width` - The architecture pointer size.
    /// - `max_value_width` - The maximum pointee width for any register. Determines the size of the
    ///   Error variant.
    fn error_variant_def(&self, max_value_width: model::PtrSize) -> Option<TokenStream> {
        let max_value_width = format_ident!(
            "{}",
            bit_count_to_rust_uint_type_str(max_value_width.bit_count())
        );
        match self {
            Self::Read => None,
            Self::ReadIsResetVal => Some(
                // We try to avoid generating extra code as much as possible. Therefore we only
                // reference existing static symbols or integers here.
                quote! {
                    /// The read value was not the expected value on reset
                    ///
                    /// This means that the source file (IP-XACT or SVD) claims that the register
                    /// value on reset should be `reset_val` but it was `read_val` instead.
                    ReadValueIsNotResetValue {
                        /// The value that was read from the register
                        read_val: #max_value_width,
                        /// Expected value for this register on reset
                        reset_val: #max_value_width,
                        /// Register identifier or "full path"
                        reg_uid: &'static str,
                        /// Register address
                        reg_addr: usize,
                    },
                },
            ),
        }
    }
}

impl str::FromStr for RegTestKind {
    type Err = ParseTestKindError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "read" => Ok(Self::Read),
            "reset" | "read_is_reset_val" => Ok(Self::ReadIsResetVal),
            s => Err(ParseTestKindError(s.to_owned())),
        }
    }
}

#[derive(Clone, Debug)]
pub enum FailureImplKind {
    None,
    Panic,
    ReturnError,
}

#[derive(Clone, Debug)]
pub struct TestConfig {
    /// What types of tests to generate
    ///
    /// [RegTestKind::Read]: read register value (may cause e.g., bus failure or hang)
    /// [RegTestKind::Reset]: read register value and verify it matches with reset value
    tests_to_generate: HashSet<RegTestKind>,
    /// What to do on failure:
    ///
    /// [FailureImplementation::ReturnValue]: just return the possibly incorrect value
    /// [FailureImplementation::Panic]: panic on failure (e.g., through assert)
    on_fail: FailureImplKind,
    /// Generate `#[derive(Debug)]` for types such as Error
    derive_debug: bool,
    /// Ignore reset masks when checking for reset value match
    ///
    /// This is useful because sometimes the people who make SVDs make all the
    /// reset masks zeros and we need to ignore them.
    force_ignore_reset_mask: bool,
    arch_ptr_size: model::PtrSize,
}

impl TestConfig {
    #[must_use]
    pub fn new(archi_ptr_size: model::PtrSize) -> Self {
        Self {
            tests_to_generate: iter::once(RegTestKind::Read).collect(),
            on_fail: FailureImplKind::ReturnError,
            derive_debug: false,
            // HACK: set to true on defautl while Headsail's reset masks are broken
            force_ignore_reset_mask: true,
            arch_ptr_size: archi_ptr_size,
        }
    }

    /// Get test kinds
    ///
    /// # Errors
    ///
    /// - Read test was not enabled while reset test was
    pub fn tests_to_generate(
        mut self,
        tests_to_generate: HashSet<RegTestKind>,
    ) -> Result<Self, GenerateError> {
        if tests_to_generate.contains(&RegTestKind::ReadIsResetVal)
            && !tests_to_generate.contains(&RegTestKind::Read)
        {
            return Err(GenerateError::InvalidConfig {
                c: self,
                cause: "enabling of reset test requires read test to be enabled as well".to_owned(),
            });
        }
        self.tests_to_generate = tests_to_generate;
        Ok(self)
    }

    /// TODO
    ///
    /// # Errors
    ///
    /// - TODO
    pub fn on_fail(mut self, on_fail: FailureImplKind) -> Result<Self, GenerateError> {
        self.on_fail = on_fail;
        Ok(self)
    }

    pub fn archi_ptr_size(&self) -> model::PtrSize {
        self.arch_ptr_size
    }
}

/// Generates a "bitwise and" operation for given value and mask
///
/// # Examples
///
/// `val = 0xb0`, `mask = u8::MAX` -> `0xb0u8`
/// `val = 0xb0`, `mask = 1` ->  `(0xb0u8 & 0b1u8)`
fn gen_bitand<T: model::BitSized<T> + fmt::LowerHex + fmt::Binary + cmp::PartialOrd>(
    value: T,
    mask: T,
) -> TokenStream {
    let value_lit = u_to_hexlit(value, T::bit_count());

    match mask {
        mask if mask == T::all_ones() => {
            quote!(#value_lit)
        }
        mask => {
            let mask_bin = u_to_binlit(mask, T::bit_count());
            quote! {
                (#value_lit & #mask_bin)
            }
        }
    }
}

/// Get a literal hexadecimal representation of `val`, e.g., "0xdeadbeef"
fn u_to_hexlit<T: fmt::LowerHex + cmp::PartialOrd + model::BitSized<T>>(
    val: T,
    bits: u32,
) -> String {
    assert!(
        T::can_represent(val),
        "value `{val:?}` cannot be represented using `{}`",
        any::type_name::<T>()
    );
    match bits {
        8 => format!("{val:#x}u8"),
        16 => format!("{val:#x}u16"),
        32 => format!("{val:#x}u32"),
        64 => format!("{val:#x}u64"),
        b => panic!("invalid bit count for literal: {b}"),
    }
}

/// Get a literal binary representation of `val`, e.g., "0b10101010"
fn u_to_binlit<T: fmt::Binary + cmp::PartialOrd + model::BitSized<T>>(val: T, bits: u32) -> String {
    assert!(
        T::can_represent(val),
        "value `{val:?}` cannot be represented using `{}`",
        any::type_name::<T>()
    );
    match bits {
        8 => format!("{val:#b}u8"),
        16 => format!("{val:#b}u16"),
        32 => format!("{val:#b}u32"),
        64 => format!("{val:#b}u64"),
        b => panic!("invalid bit count for literal: {b}"),
    }
}

#[test]
fn reset_value_bitands_generate() {
    assert_eq!(
        &ResetValue::U8 {
            val: 0xb0,
            mask: u8::MAX
        }
        .gen_bitand()
        .to_string(),
        "0xb0u8"
    );
    assert_eq!(
        &ResetValue::U8 {
            val: 0xb0,
            mask: 0b0000_0001,
        }
        .gen_bitand()
        .to_string(),
        "(0xb0u8 & 0b1u8)"
    );
    assert_eq!(
        &ResetValue::U8 {
            val: 0xb0,
            mask: 0b0000_0010,
        }
        .gen_bitand()
        .to_string(),
        "(0xb0u8 & 0b10u8)"
    );
    assert_eq!(
        &ResetValue::U32 {
            val: 0xdead_beef,
            mask: 0b0101_0101,
        }
        .gen_bitand()
        .to_string(),
        "(0xdeadbeefu32 & 0b1010101u32)"
    );
    assert_eq!(
        &ResetValue::U32 {
            val: 0xdead_beef,
            mask: u32::MAX,
        }
        .gen_bitand()
        .to_string(),
        "0xdeadbeefu32"
    );
    assert_eq!(
        &ResetValue::U64 {
            val: 0xdead_beef_cafe_f00d,
            mask: u64::MAX,
        }
        .gen_bitand()
        .to_string(),
        "0xdeadbeefcafef00du64"
    );
}

pub(crate) fn bit_count_to_rust_uint_type_str(bit_count: u32) -> &'static str {
    match bit_count {
        8 => "u8",
        16 => "u16",
        32 => "u32",
        64 => "u64",
        _ => panic!("{bit_count} is not a valid bit count"),
    }
}

/// Generates test cases based on a [`TestRegister`] definition and [`TestConfig`]
///
/// Test cases are represented by [`TokenStream`] which can be rendered to text.
/// This text is then compiled as Rust source code.
struct RegTestGenerator<'r, 'c, P: ArchPtr + quote::IdentFragment + 'static>(
    &'r dyn TestRegister<P>,
    &'c TestConfig,
);

impl<'r, 'c, P: ArchPtr + quote::IdentFragment> RegTestGenerator<'r, 'c, P> {
    /// Name for the binding to the pointer to the memory mapped register
    fn ptr_binding() -> TokenStream {
        quote!(reg_ptr)
    }

    /// Name for the binding to the value that was read from the register
    ///
    /// We prefix it with underscore to avoid a warning about unused, since we
    /// may or may not want to re-use the binding depending on which tests get
    /// generated for this particular register
    fn read_value_binding() -> TokenStream {
        quote!(_read_value)
    }

    /// Create a [`RegTestGenerator`] from a register definition
    pub fn from_register(reg: &'r impl TestRegister<P>, config: &'c TestConfig) -> Self {
        Self(reg, config)
    }

    /// Generates a test that reads an appropriate amount of bytes from the
    /// register
    fn gen_read_test(&self) -> TokenStream {
        let ptr_binding = Self::ptr_binding();
        let read_value_binding = Self::read_value_binding();
        quote! {
            let #read_value_binding = unsafe { read_volatile(#ptr_binding) };
        }
    }

    /// Generates a function identifier
    ///
    /// # Examples
    ///
    /// * `test_MCR_0xfff00004`
    /// * `test_MDIO_RD_DATA_0xff40040c`
    fn gen_test_fn_ident(&self) -> Ident {
        let reg = &self.0;
        let addr: P = reg.addr();
        format_ident!("test_{}_{:#x}", reg.name(), addr)
    }

    /// Generates a test function
    ///
    /// Example output:
    ///
    /// ```rust,ignore
    /// pub fn test_something_0xdeadbeef() {
    ///     #[allow(unused)]
    ///     let reg_ptr: *mut u32 = 0xdeadbeef as *mut u32;
    ///
    ///     let _read_value = unsafe { read_volatile(reg_ptr) };
    /// }
    /// ```
    pub fn gen_test_fn(&self) -> Result<TokenStream, GenerateError> {
        let (reg, config) = (&self.0, self.1);

        // Name for the variable holding the pointer to the register
        let ptr_binding = Self::ptr_binding();
        let reg_size_ty = format_ident!("{}", bit_count_to_rust_uint_type_str(reg.size()));
        let addr_hex: TokenStream = format!("{:#x}", reg.addr()).parse().unwrap();

        let fn_name = self.gen_test_fn_ident();

        // Only generate read test if register is readable
        let gen_read_test =
            reg.is_readable() && config.tests_to_generate.contains(&RegTestKind::Read);
        let read_test = gen_read_test
            .then(|| self.gen_read_test())
            .unwrap_or_default();

        // Only generate reset value test if register is readable and has a reset value
        let gen_reset_test = reg.is_readable()
            && config
                .tests_to_generate
                .contains(&RegTestKind::ReadIsResetVal);
        let reset_val_test = gen_reset_test
            .then(|| {
                reg.reset_value()
                    .map(|value_on_reset| {
                        gen_read_is_reset_val_test(
                            reg.uid(),
                            reg.addr(),
                            reg.size(),
                            &value_on_reset,
                            config,
                        )
                    })
                    .unwrap_or_default()
            })
            .unwrap_or_default();

        let ret = quote! {
            #[allow(non_snake_case)]
            pub fn #fn_name() -> Result<()> {
                #[allow(unused)]
                let #ptr_binding: *mut #reg_size_ty = #addr_hex as *mut #reg_size_ty;

                #read_test
                #reset_val_test
                Ok(())
            }
        };
        Ok(ret)
    }

    /// Generates a test definition that can be put into an array initializer
    ///
    /// Example output:
    ///
    /// ```rust,ignore
    /// TestCase {
    ///     function: foo::test_something_0xdeafbeef,
    ///     addr: 0xdeafbeef,
    ///     uid: "test_something",
    /// }
    /// ```
    pub fn gen_test_def(&self) -> TokenStream {
        let fn_name = self.gen_test_fn_ident();
        let periph_name_lc: TokenStream = self.0.periph_name().to_lowercase().parse().unwrap();
        let func = quote!(#periph_name_lc::#fn_name);
        let addr_hex: TokenStream = format!("{:#x}", self.0.addr()).parse().unwrap();
        let uid = self.0.uid();

        quote! {
            TestCase {
                function: #func,
                addr: #addr_hex,
                uid: #uid,
            }
        }
    }
}

/// Generates a test that verifies that the read value matches with reported
/// reset value
///
/// # Arguments
///
/// * `uid` - Register UID
/// * `size` - The size of the register in bits
/// * `reset_value` - A masked reset value that will be checked against
fn gen_read_is_reset_val_test<P: ArchPtr + quote::IdentFragment + 'static>(
    uid: String,
    addr: P,
    size: u32,
    reset_value: &ValueOnReset<u64>,
    config: &TestConfig,
) -> TokenStream {
    // Reset value test requires read test to be present. Can't check for
    // reset value unless it's been read before.
    debug_assert!(config.tests_to_generate.contains(&RegTestKind::Read));

    let read_value_binding = RegTestGenerator::<P>::read_value_binding();
    let reset_val_frag = if config.force_ignore_reset_mask || reset_value.mask.is_none() {
        u_to_hexlit(reset_value.value, size).parse().unwrap()
    } else {
        // Unwrap: checked on conditional
        gen_bitand(reset_value.value, reset_value.mask.unwrap())
    };
    let reg_size_ty = format_ident!("{}", bit_count_to_rust_uint_type_str(size));

    match config.on_fail {
        // If reset value is incorrect, panic
        FailureImplKind::Panic => quote! {
            assert_eq!(#read_value_binding, #reset_val_frag as #reg_size_ty);
        },
        // If reset value is incorrect, do nothing
        FailureImplKind::None => quote! {},
        FailureImplKind::ReturnError => {
            let addr_hex: TokenStream = format!("{:#x}", addr).parse().unwrap();
            let max_val_width = quote!(u64);
            quote! {
                if #read_value_binding != #reset_val_frag as #reg_size_ty {
                    return Err(Error::ReadValueIsNotResetValue {
                            read_val: #read_value_binding as #max_val_width,
                            reset_val: #reset_val_frag,
                            reg_uid: #uid,
                            reg_addr: #addr_hex,
                        })
                }
            }
        }
    }
}

/// Collection of all test cases for this build.
pub struct TestCases {
    pub preamble: String,
    pub test_cases: Vec<String>,
    pub test_case_count: usize,
}

impl TestCases {
    /// Generate test cases for each register.
    ///
    /// # Panics
    ///
    /// - Failed to parse token stream
    ///
    /// # Errors
    ///
    /// - Failed to generate test case for a register
    pub fn from_registers<P: ArchPtr + quote::IdentFragment + 'static>(
        registers: &Registers<P, model::RefSchemaSvdV1_2>,
        config: &TestConfig,
    ) -> Result<Self, GenerateError> {
        let preamble = gen_preamble(config.arch_ptr_size, config.derive_debug).to_string();

        let mut test_fns_and_defs_by_periph: HashMap<String, Vec<(String, String)>> =
            HashMap::new();
        for register in registers.iter() {
            let test_gen = RegTestGenerator::from_register(register, config);
            let test_fn = test_gen.gen_test_fn()?.to_string();
            let test_def = test_gen.gen_test_def().to_string();
            test_fns_and_defs_by_periph
                .entry(register.path.periph().name.clone())
                .or_default()
                .push((test_fn, test_def));
        }

        // Duplicate all test definitions into one big list
        let test_defs = test_fns_and_defs_by_periph
            .values()
            .flatten()
            .map(|(_func, def)| def)
            .cloned()
            .collect_vec();
        let test_case_count = test_defs.len();
        let test_defs_combined: TokenStream = test_defs.join(",").parse().unwrap();
        let test_case_array = quote! {
            pub static TEST_CASES: [TestCase; #test_case_count] = [ #test_defs_combined ];
        };

        let mod_strings = create_modules(test_fns_and_defs_by_periph)
            .into_iter()
            .map(|ts| ts.to_string())
            .collect::<String>();

        Ok(Self {
            preamble,
            test_cases: vec![mod_strings, format!("{test_case_array}")],
            test_case_count,
        })
    }

    #[must_use]
    pub fn to_module_string(&self) -> String {
        self.preamble.clone() + "\n" + &self.test_cases.join("\n")
    }
}
