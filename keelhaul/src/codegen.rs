//! Generate test cases from types implementing [`TestRegister`]

// Codegen API
pub use self::test_register::{ParseTestKindError, TestRegister, ValueOnReset};

mod test_register;

use std::{
    any, cmp,
    collections::{HashMap, HashSet},
    fmt, iter, str,
};

use self::test_register::RegTestGenerator;
use crate::{
    api,
    model::{self, ArchPtr, Registers},
    TestKind,
};
use itertools::Itertools;
use proc_macro2::TokenStream;
use quote::quote;
use strum::IntoEnumIterator;

fn gen_preamble(arch: model::PtrSize, error_derive_debug: bool) -> TokenStream {
    // It costs a lot of code size to `#[derive(Debug)]` so we only do it if required
    let opt_derive_debug = if error_derive_debug {
        quote!(#[derive(Debug)])
    } else {
        quote!()
    };

    // Generate an error variant for all test case kinds
    let error_variant_defs: TokenStream = TestKind::iter()
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
    tests_to_generate: HashSet<TestKind>,
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
    arch_ptr_size: api::ArchWidth,
}

impl TestConfig {
    #[must_use]
    pub fn new(arch_ptr_size: api::ArchWidth) -> Self {
        Self {
            tests_to_generate: iter::once(TestKind::Read).collect(),
            on_fail: FailureImplKind::ReturnError,
            derive_debug: false,
            // HACK: set to true on defautl while Headsail's reset masks are broken
            force_ignore_reset_mask: true,
            arch_ptr_size,
        }
    }

    /// Get test kinds
    ///
    /// # Errors
    ///
    /// - Read test was not enabled while reset test was
    pub fn tests_to_generate(
        mut self,
        tests_to_generate: HashSet<TestKind>,
    ) -> Result<Self, String> {
        if tests_to_generate.contains(&TestKind::ReadIsResetVal)
            && !tests_to_generate.contains(&TestKind::Read)
        {
            return Err(
                "enabling of reset test requires read test to be enabled as well".to_owned(),
            );
        }
        self.tests_to_generate = tests_to_generate;
        Ok(self)
    }

    /// TODO
    ///
    /// # Errors
    ///
    /// - TODO
    pub fn on_fail(mut self, on_fail: FailureImplKind) -> Self {
        self.on_fail = on_fail;
        self
    }

    pub fn derive_debug(mut self, derive_debug: bool) -> Self {
        self.derive_debug = derive_debug;
        self
    }

    pub fn ignore_reset_masks(mut self, ignore_reset_masks: bool) -> Self {
        self.force_ignore_reset_mask = ignore_reset_masks;
        self
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
) -> TokenStream {
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
    .parse()
    .unwrap()
}

/// Get a literal binary representation of `val`, e.g., "0b10101010"
fn u_to_binlit<T: fmt::Binary + cmp::PartialOrd + model::BitSized<T>>(
    val: T,
    bits: u32,
) -> TokenStream {
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
    .parse()
    .unwrap()
}

#[test]
fn reset_value_bitands_generate() {
    use crate::codegen::gen_bitand;

    assert_eq!(gen_bitand(0xb0, u8::MAX).to_string(), "0xb0u8");
    assert_eq!(
        gen_bitand(0xb0, 0b0000_0001u8).to_string(),
        "(0xb0u8 & 0b1u8)"
    );
    assert_eq!(
        gen_bitand(0xb0, 0b0000_0010u8).to_string(),
        "(0xb0u8 & 0b10u8)"
    );
    assert_eq!(
        gen_bitand(0xdead_beef, 0b0101_0101u32).to_string(),
        "(0xdeadbeefu32 & 0b1010101u32)"
    );
    assert_eq!(
        gen_bitand(0xdead_beef, u32::MAX).to_string(),
        "0xdeadbeefu32"
    );
    assert_eq!(
        gen_bitand(0xdead_beef_cafe_f00d, u64::MAX).to_string(),
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
    ) -> Self {
        let preamble = gen_preamble(config.arch_ptr_size.into(), config.derive_debug).to_string();

        let mut test_fns_and_defs_by_periph: HashMap<String, Vec<(String, String)>> =
            HashMap::new();
        for register in registers.iter() {
            let test_gen = RegTestGenerator::from_register(register, config);
            let test_fn = test_gen.gen_test_fn().to_string();
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

        Self {
            preamble,
            test_cases: vec![mod_strings, format!("{test_case_array}")],
            test_case_count,
        }
    }

    #[must_use]
    pub fn to_module_string(&self) -> String {
        self.preamble.clone() + "\n" + &self.test_cases.join("\n")
    }
}
