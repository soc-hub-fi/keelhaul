//! Generate test cases from [`model::*`] types

// TODO: maybe generate array registers under new module-level

use crate::{
    error::GenerateError,
    model::{self, ArchPtr, PtrSize, RegValue, Register, Registers, ResetValue},
};
use itertools::Itertools;
use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote};
use std::{
    collections::{HashMap, HashSet},
    iter, str,
};
use strum::{EnumIter, IntoEnumIterator};
use thiserror::Error;

// TODO: REMOVE
/*
/// Remove illegal characters from register name.
///
/// These characters will be put into test case names, and thus need to be removed.
fn _remove_illegal_characters(name: &str) -> String {
    let mut name_new = name.to_owned();
    const ILLEGALS: &[char] = &['(', ')', '[', ']', '%'];
    let mut found_illegals = Vec::new();
    for illegal in ILLEGALS {
        if name_new.contains(*illegal) {
            found_illegals.push(illegal);
            name_new = name_new.replace(*illegal, "_");
        }
    }
    if !found_illegals.is_empty() {
        let symbols = found_illegals
            .iter()
            .map(|c| format!("\"{}\"", c))
            .join(", ");
        warn!(
            "Register {}'s name contains {} illegal characters: {}. These characters are replaced with underscores ('_').",
            name,
            found_illegals.len(),
            symbols
        );
    }
    name_new
}
*/

fn gen_preamble(config: &TestConfig) -> TokenStream {
    // It costs a lot of code size to `#[derive(Debug)]` so we only do it if required
    let opt_derive_debug = if config.derive_debug {
        quote!(#[derive(Debug)])
    } else {
        quote!()
    };

    // Generate an error variant for all test case kinds
    let error_variant_defs: TokenStream = RegTestKind::iter()
        .filter_map(|test_kind| test_kind.error_variant_def(config.archi_ptr_size()))
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

#[derive(Clone, Debug, Hash, PartialEq, Eq, EnumIter)]
pub enum RegTestKind {
    /// The register value will be read
    Read,
    /// The read value will be compared against the reset value
    ///
    /// Only generated when the reset value is reported in source format
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
    fn error_variant_def(&self, max_value_width: PtrSize) -> Option<TokenStream> {
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
    reg_test_kinds: HashSet<RegTestKind>,
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
    archi_ptr_size: PtrSize,
}

impl TestConfig {
    #[must_use]
    pub fn new(archi_ptr_size: PtrSize) -> Self {
        Self {
            reg_test_kinds: iter::once(RegTestKind::Read).collect(),
            on_fail: FailureImplKind::ReturnError,
            derive_debug: false,
            // HACK: set to true on defautl while Headsail's reset masks are broken
            force_ignore_reset_mask: true,
            archi_ptr_size,
        }
    }

    /// Get test kinds
    ///
    /// # Errors
    ///
    /// - Read test was not enabled while reset test was
    pub fn reg_test_kinds(
        mut self,
        reg_test_kinds: HashSet<RegTestKind>,
    ) -> Result<Self, GenerateError> {
        if reg_test_kinds.contains(&RegTestKind::ReadIsResetVal)
            && !reg_test_kinds.contains(&RegTestKind::Read)
        {
            return Err(GenerateError::InvalidConfig {
                c: self,
                cause: "enabling of reset test requires read test to be enabled as well".to_owned(),
            });
        }
        self.reg_test_kinds = reg_test_kinds;
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

    pub fn archi_ptr_size(&self) -> PtrSize {
        self.archi_ptr_size
    }
}

impl ResetValue {
    /// Generates a "bitwise and" operation for reset value that can be used to
    /// compare to the value received from the register
    ///
    /// # Examples
    ///
    /// `val = 0xb0`, `mask = u8::MAX` -> `0xb0u8`
    /// `val = 0xb0`, `mask = 1` ->  `(0xb0u8 & 0b1u8)`
    fn gen_bitand(&self) -> TokenStream {
        let (value, mask) = (self.value(), self.mask());
        let value_lit = value.gen_literal_hex();

        match mask {
            mask if mask == mask.width().max_value() => {
                quote!(#value_lit)
            }
            mask => {
                let mask_bin = mask.gen_literal_bin();
                quote! {
                    (#value_lit & #mask_bin)
                }
            }
        }
    }
}

impl RegValue {
    fn gen_literal_hex(&self) -> TokenStream {
        match self {
            Self::U8(u) => format!("{u:#x}u8"),
            Self::U16(u) => format!("{u:#x}u16"),
            Self::U32(u) => format!("{u:#x}u32"),
            Self::U64(u) => format!("{u:#x}u64"),
        }
        .parse()
        .unwrap()
    }

    fn gen_literal_bin(&self) -> TokenStream {
        match self {
            Self::U8(u) => format!("{u:#b}u8"),
            Self::U16(u) => format!("{u:#b}u16"),
            Self::U32(u) => format!("{u:#b}u32"),
            Self::U64(u) => format!("{u:#b}u64"),
        }
        .parse()
        .unwrap()
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

/// Generates test cases based on a [`Register`] definition and [`TestConfig`]
///
/// Test cases are represented by [`TokenStream`] which can be rendered to text.
/// This text is then compiled as Rust source code.
struct RegTestGenerator<'r, 'c, P: ArchPtr + quote::IdentFragment + 'static>(
    &'r Register<P, model::RefSchemaSvdV1_2>,
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
    pub const fn from_register(
        reg: &'r Register<P, model::RefSchemaSvdV1_2>,
        config: &'c TestConfig,
    ) -> Self {
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
    fn gen_test_fn_ident(&self) -> Result<Ident, GenerateError> {
        let reg = self.0;
        let full_addr: Result<P, _> = reg.full_addr();
        Ok(format_ident!(
            "test_{}_{:#x}",
            reg.path.reg().name,
            full_addr?
        ))
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
        let (reg, config) = (self.0, self.1);

        // Name for the variable holding the pointer to the register
        let ptr_binding = Self::ptr_binding();
        let reg_size_ty = format_ident!("{}", bit_count_to_rust_uint_type_str(reg.size));
        let addr_hex: TokenStream = format!("{:#x}", reg.full_addr()?).parse().unwrap();

        let fn_name = self.gen_test_fn_ident()?;

        // Only generate read test if register is readable
        let read_test = if reg.is_readable() && config.reg_test_kinds.contains(&RegTestKind::Read) {
            self.gen_read_test()
        } else {
            quote!()
        };

        // Only generate reset value test if register is readable
        let reset_val_test = if self.0.is_readable()
            && config.reg_test_kinds.contains(&RegTestKind::ReadIsResetVal)
            && u64::from(self.0.masked_reset().mask()) != 0u64
        {
            gen_reset_val_test(
                self.0.uid(),
                self.0.full_addr()?,
                self.0.size,
                *self.0.masked_reset(),
                config,
            )?
        } else {
            quote!()
        };

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
    pub fn gen_test_def(&self) -> Result<TokenStream, GenerateError> {
        let fn_name = self.gen_test_fn_ident()?;
        let periph_name_lc: TokenStream = self.0.path.periph().name.to_lowercase().parse().unwrap();
        let func = quote!(#periph_name_lc::#fn_name);
        let addr_hex: TokenStream = format!("{:#x}", self.0.full_addr()?).parse().unwrap();
        let uid = self.0.uid();

        let def = quote! {
            TestCase {
                function: #func,
                addr: #addr_hex,
                uid: #uid,
            }
        };
        Ok(def)
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
fn gen_reset_val_test<P: ArchPtr + quote::IdentFragment + 'static>(
    uid: String,
    addr: P,
    size: u32,
    reset_value: model::ResetValue,
    config: &TestConfig,
) -> Result<TokenStream, GenerateError> {
    // Reset value test requires read test to be present. Can't check for
    // reset value unless it's been read before.
    debug_assert!(config.reg_test_kinds.contains(&RegTestKind::Read));

    let read_value_binding = RegTestGenerator::<P>::read_value_binding();
    let reset_val_frag = if config.force_ignore_reset_mask {
        reset_value.value().gen_literal_hex()
    } else {
        reset_value.gen_bitand()
    };
    let reg_size_ty = format_ident!("{}", bit_count_to_rust_uint_type_str(size));

    match config.on_fail {
        // If reset value is incorrect, panic
        FailureImplKind::Panic => Ok(quote! {
            assert_eq!(#read_value_binding, #reset_val_frag as #reg_size_ty);
        }),
        // If reset value is incorrect, do nothing
        FailureImplKind::None => Ok(quote! {}),
        FailureImplKind::ReturnError => {
            let addr_hex: TokenStream = format!("{:#x}", addr).parse().unwrap();
            let max_val_width = quote!(u64);
            Ok(quote! {
                if #read_value_binding != #reset_val_frag as #reg_size_ty {
                    return Err(Error::ReadValueIsNotResetValue {
                            read_val: #read_value_binding as #max_val_width,
                            reset_val: #reset_val_frag,
                            reg_uid: #uid,
                            reg_addr: #addr_hex,
                        })
                }
            })
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
        let preamble = gen_preamble(config).to_string();

        let mut test_fns_and_defs_by_periph: HashMap<String, Vec<(String, String)>> =
            HashMap::new();
        for register in registers.iter() {
            let test_gen = RegTestGenerator::from_register(register, config);
            let test_fn = test_gen.gen_test_fn()?.to_string();
            let test_def = test_gen.gen_test_def()?.to_string();
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
