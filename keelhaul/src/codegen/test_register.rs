use std::collections::HashMap;

use crate::{
    codegen,
    model::{self, ArchPtr, UniquePath},
    CodegenConfig, TestKind,
};
use itertools::Itertools;
use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote};

/// Type that a test can be generated for
pub(crate) trait TestRegister: model::UniquePath {
    /// Get the absolute memory address of the register
    fn addr(&self) -> u64;

    /// The size of the register in bits
    fn size(&self) -> u32;

    /// Whether reading the register has a defined effect
    fn is_readable(&self) -> bool;

    /// An optional, known reset value
    fn reset_value(&self) -> Option<ValueOnReset<u64>>;

    /// A human-readable unique identifier for the register, usually the the path that is used to
    /// access the register.
    fn binding_id(&self) -> String {
        self.path().join("_")
    }

    /// The name of the register, usually the final element of the `path`
    fn name(&self) -> String {
        self.path().last().unwrap().to_owned()
    }
}

#[derive(Clone, Debug)]
pub struct ValueOnReset<T> {
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

/// Generates test cases based on a [`TestRegister`] definition and [`TestConfig`]
///
/// Test cases are represented by [`TokenStream`] which can be rendered to text.
/// This text is then compiled as Rust source code.
pub(crate) struct RegTestGenerator<'r, 'c>(&'r dyn TestRegister, &'c CodegenConfig);

impl<'r, 'c> RegTestGenerator<'r, 'c> {
    /// Name for the binding to the pointer to the memory mapped register
    fn ptr_binding() -> TokenStream {
        quote!(_reg_ptr)
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
    pub fn from_register(reg: &'r impl TestRegister, config: &'c CodegenConfig) -> Self {
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
        format_ident!("test_{}_{:#x}", reg.name(), reg.addr())
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
    pub fn gen_test_fn(&self) -> TokenStream {
        let (reg, config) = (&self.0, self.1);

        // Name for the variable holding the pointer to the register
        let ptr_binding = Self::ptr_binding();
        let reg_size_ty = format_ident!("{}", codegen::bit_count_to_rust_uint_type_str(reg.size()));
        let addr_hex: TokenStream = format!("{:#x}", reg.addr()).parse().unwrap();

        let fn_name = self.gen_test_fn_ident();

        // Only generate read test if register is readable
        let gen_read_test = reg.is_readable() && config.tests_to_generate.contains(&TestKind::Read);
        let read_test = gen_read_test
            .then(|| self.gen_read_test())
            .unwrap_or_default();

        // Only generate reset value test if register is readable and has a reset value
        let gen_reset_test =
            reg.is_readable() && config.tests_to_generate.contains(&TestKind::ReadIsResetVal);
        let reset_val_test = gen_reset_test
            .then(|| {
                reg.reset_value()
                    .map(|value_on_reset| {
                        gen_read_is_reset_val_test(
                            reg.binding_id(),
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
                let #ptr_binding: *mut #reg_size_ty = #addr_hex as *mut #reg_size_ty;

                #read_test
                #reset_val_test
                Ok(())
            }
        };
        ret
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
        let periph_name_lc: TokenStream =
            self.0.top_container_name().to_lowercase().parse().unwrap();
        let func = quote!(#periph_name_lc::#fn_name);
        let addr_hex: TokenStream = format!("{:#x}", self.0.addr()).parse().unwrap();
        let uid = self.0.binding_id();

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
    config: &CodegenConfig,
) -> TokenStream {
    // Reset value test requires read test to be present. Can't check for
    // reset value unless it's been read before.
    debug_assert!(config.tests_to_generate.contains(&TestKind::Read));

    let read_value_binding = RegTestGenerator::read_value_binding();
    let reset_val_frag = if config.force_ignore_reset_mask || reset_value.mask.is_none() {
        codegen::u_to_hexlit(reset_value.value, size)
    } else {
        // Unwrap: checked on conditional
        codegen::gen_bitand(reset_value.value, reset_value.mask.unwrap())
    };
    let reg_size_ty = format_ident!("{}", codegen::bit_count_to_rust_uint_type_str(size));

    match config.on_fail {
        // If reset value is incorrect, panic
        codegen::FailureImplKind::Panic => quote! {
            assert_eq!(#read_value_binding, #reset_val_frag as #reg_size_ty);
        },
        // If reset value is incorrect, do nothing
        codegen::FailureImplKind::None => quote! {},
        codegen::FailureImplKind::ReturnError => {
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

/// Place test cases in modules.
fn gen_test_mod_wrapper(
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
pub struct RegTestCases {
    preamble: TokenStream,
    test_cases: Vec<TokenStream>,
    test_case_array: TokenStream,
    pub(crate) test_case_count: usize,
}

impl RegTestCases {
    /// Generate test cases for each register.
    ///
    /// # Panics
    ///
    /// - Failed to parse token stream
    ///
    /// # Errors
    ///
    /// - Failed to generate test case for a register
    pub fn from_registers(registers: &model::Registers, config: &CodegenConfig) -> Self {
        let widest = registers.iter().map(|reg| reg.size()).max().unwrap();
        let preamble = codegen::gen_preamble(widest, config.derive_debug);

        let mut test_fns_and_defs_by_periph: HashMap<String, Vec<(String, String)>> =
            HashMap::new();
        for register in registers.iter() {
            let test_gen = RegTestGenerator::from_register(register, config);
            let test_fn = test_gen.gen_test_fn().to_string();
            let test_def = test_gen.gen_test_def().to_string();
            test_fns_and_defs_by_periph
                .entry(register.top_container_name())
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

        let test_cases = gen_test_mod_wrapper(test_fns_and_defs_by_periph);

        Self {
            preamble,
            test_cases,
            test_case_array,
            test_case_count,
        }
    }

    pub fn to_tokens(&self) -> TokenStream {
        let RegTestCases {
            preamble,
            test_cases,
            test_case_array,
            ..
        } = self;
        quote! {
            #preamble

            #(
                #test_cases
            )*

            #test_case_array
        }
    }
}
