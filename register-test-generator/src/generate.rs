//! Generate test cases from model::* types
use crate::{GenerateError, Register, Registers};
use itertools::Itertools;
use log::warn;
use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote};
use std::{
    collections::{HashMap, HashSet},
    iter,
};

/// Remove illegal characters from register name.
///
/// These characters will be put into test case names, and thus need to be removed.
fn _remove_illegal_characters(name: &str) -> String {
    let mut name_new = name.to_owned();
    let illegals = ['(', ')', '[', ']', '%'];
    let mut found_illegals = Vec::new();
    for illegal in illegals {
        if name_new.contains(illegal) {
            found_illegals.push(illegal);
            name_new = name_new.replace(illegal, "_");
        }
    }
    if !found_illegals.is_empty() {
        let symbols = found_illegals
            .iter()
            .map(|c| format!("\"{}\"", c.to_owned()))
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

/// Collection of all test cases for this build.
pub struct TestCases {
    pub test_cases: Vec<String>,
    pub test_case_count: usize,
}

#[derive(Hash, PartialEq, Eq)]
enum RegTestKind {
    Read,
    Reset,
}

enum FailureImplementation {
    ReturnValue,
    Panic,
}

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
    on_fail: FailureImplementation,
}

impl Default for TestConfig {
    fn default() -> Self {
        Self {
            reg_test_kinds: HashSet::from_iter(iter::once(RegTestKind::Read)),
            on_fail: FailureImplementation::ReturnValue,
        }
    }
}

/// Generates test cases based on a [Register] definition and [TestConfig]
///
/// Test cases are represented by [TokenStream] which can be rendered to text.
/// This text is then compiled as Rust source code.
struct RegTestGenerator<'r, 'c>(&'r Register<u32>, &'c TestConfig);

impl<'r, 'c> RegTestGenerator<'r, 'c> {
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

    /// Create a [RegTestGenerator] from a register definition
    pub fn from_register(reg: &'r Register<u32>, config: &'c TestConfig) -> Self {
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

    /// Generates a test that verifies that the read value matches with reported
    /// reset value
    fn gen_reset_val_test(&self, config: &TestConfig) -> TokenStream {
        // Reset value test requires read test to be present. Can't check for
        // reset value unless it's been read before.
        debug_assert!(config.reg_test_kinds.contains(&RegTestKind::Read));

        let read_value_binding = Self::read_value_binding();
        let reset_val = self.0.reset_val;
        match config.on_fail {
            // If reset value is incorrect, panic
            FailureImplementation::Panic => quote! {
                assert_eq!(#read_value_binding, #reset_val);
            },
            // If reset value is incorrect, return it
            FailureImplementation::ReturnValue => quote! {
                if #read_value_binding != #reset_val {
                    return #read_value_binding;
                }
            },
        }
    }

    fn gen_test_fn_ident(&self) -> Result<Ident, GenerateError> {
        let reg = self.0;
        let full_addr: Result<u32, _> = reg.full_addr();
        Ok(format_ident!("test_{}_{:#x}", reg.path.reg, full_addr?))
    }

    /// Generates a test function
    ///
    /// Example output:
    ///
    /// ```rust,no_run
    /// pub fn test_something_0xdeadbeef() {
    ///     #[allow(unused)]
    ///     let reg_ptr =*mut u32 = 0xdeadbeef as *mut u32;
    ///
    ///     let _read_value = unsafe { read_volatile(reg_ptr) };
    /// }
    /// ```
    pub fn gen_test_fn(&self) -> Result<TokenStream, GenerateError> {
        let (reg, config) = (self.0, self.1);

        // Name for the variable holding the pointer to the register
        let ptr_binding = Self::ptr_binding();
        let reg_size_ty = format_ident!("{}", reg.size.to_rust_type_str());
        let addr_hex: TokenStream = format!("{:#x}", reg.full_addr()?).parse().unwrap();

        let fn_name = self.gen_test_fn_ident()?;

        // Only generate read test if register is readable
        let read_test =
            if reg.access.is_read() && config.reg_test_kinds.contains(&RegTestKind::Read) {
                self.gen_read_test()
            } else {
                quote!()
            };

        // Only generate reset value test if register is readable
        let reset_val_test =
            if self.0.access.is_read() && config.reg_test_kinds.contains(&RegTestKind::Reset) {
                self.gen_reset_val_test(config)
            } else {
                quote!()
            };

        let ret = quote! {
            #[allow(non_snake_case)]
            pub fn #fn_name() {
                #[allow(unused)]
                let #ptr_binding: *mut #reg_size_ty = #addr_hex as *mut #reg_size_ty;

                #read_test
                #reset_val_test
            }
        };
        Ok(ret)
    }

    /// Generates a test definition that can be put into an array initializer
    ///
    /// Example output:
    ///
    /// ```rust,no_run
    /// TestCase {
    ///     function: foo::test_something_0xdeafbeef,
    ///     addr: 0xdeafbeef,
    ///     uid: "test_something",
    /// }
    /// ```
    pub fn gen_test_def(&self) -> Result<TokenStream, GenerateError> {
        let fn_name = self.gen_test_fn_ident()?;
        let periph_name_lc: TokenStream = self.0.path.periph.to_lowercase().parse().unwrap();
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

impl TestCases {
    /// Generate test cases for each register.
    pub fn from_registers(
        registers: &Registers<u32>,
        config: &TestConfig,
    ) -> Result<TestCases, GenerateError> {
        let mut test_fns_and_defs_by_periph = HashMap::new();
        for register in registers.iter() {
            let test_gen = RegTestGenerator::from_register(register, config);

            let test_fn = test_gen.gen_test_fn()?;
            let test_fn_str = format!("{test_fn}");

            let test_def = test_gen.gen_test_def()?;
            let test_def_str = format!("{test_def}");

            test_fns_and_defs_by_periph
                .entry(register.path.periph.clone())
                .or_insert(vec![])
                .push((test_fn_str, test_def_str));
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

        Ok(TestCases {
            test_cases: vec![mod_strings, format!("{test_case_array}")],
            test_case_count,
        })
    }
}
