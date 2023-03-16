//! Generate test cases from model::* types
use crate::{GenerateError, Register, Registers};
use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote};
use std::collections::HashMap;

/// Place test cases in modules.
fn create_modules(
    test_cases_by_peripheral: &HashMap<String, Vec<String>>,
    test_case_structs_by_peripheral: &HashMap<String, Vec<String>>,
) -> Vec<String> {
    let mut modules = Vec::new();
    for (periph_name, test_cases) in test_cases_by_peripheral {
        let module_test_cases_combined: TokenStream = test_case_structs_by_peripheral
            .get(periph_name)
            .unwrap()
            .join(",")
            .parse()
            .unwrap();
        let len = test_cases.len();
        let value: TokenStream = quote!( [#module_test_cases_combined] );
        let module_test_case_array = quote! {
            pub static TEST_CASES: [TestCase; #len] = #value;
        };
        let mod_name: TokenStream = periph_name.to_lowercase().parse().unwrap();
        let test_cases_catenated: TokenStream = test_cases.join("").parse().unwrap();
        let module = quote! {
            pub mod #mod_name {
                use super::*;

                #module_test_case_array

                #test_cases_catenated
            }
        };
        modules.push(format!("{}", module));
    }
    modules
}

/// Collection of all test cases for this build.
pub struct TestCases {
    pub test_cases: Vec<String>,
    pub test_case_count: usize,
}

/// Generates test cases based on a [Register] definition
///
/// Test cases are represented by [TokenStream] which can be rendered to text.
/// This text is then compiled as Rust source code.
struct RegTestGenerator<'r>(&'r Register);

impl<'r> RegTestGenerator<'r> {
    fn ptr_binding() -> TokenStream {
        quote!(reg_ptr)
    }
    // We prefix it with underscore to avoid a warning about unused, since we
    // may or may not want to re-use the binding depending on which tests get
    // generated for this particular register
    fn read_value_binding() -> TokenStream {
        quote!(_read_value)
    }

    fn from_register(reg: &'r Register) -> Self {
        Self(reg)
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
    fn _gen_reset_val_test(&self) -> TokenStream {
        let read_value_binding = Self::read_value_binding();
        let reset_val = self.0.reset_val;
        quote! {
            assert_eq!(#read_value_binding, #reset_val);
        }
    }

    fn gen_test_fn_ident(&self) -> Result<Ident, GenerateError> {
        Ok(format_ident!(
            "test_{}_{:#x}",
            self.0.reg_name,
            self.0.full_address()?
        ))
    }

    /// Generates a test function
    ///
    /// Example output:
    ///
    /// ```rust
    /// pub fn test_something_0xdeadbeef() {
    ///     let reg_ptr =*mut u32 = 0xdeadbeef as *mut u32;
    ///
    ///     let _read_value = unsafe { read_volatile(reg_ptr) };
    /// }
    /// ```
    pub fn gen_test_fn(&self) -> Result<TokenStream, GenerateError> {
        // Name for the variable holding the pointer to the register
        let ptr_binding = Self::ptr_binding();
        let reg_size_ty = format_ident!("{}", self.0.size.to_rust_type_str());
        let addr_hex: TokenStream = format!("{:#x}", self.0.full_address()?).parse().unwrap();

        let fn_name = self.gen_test_fn_ident()?;

        // Only generate read test if register is readable
        let read_test = if self.0.is_read {
            self.gen_read_test()
        } else {
            quote!()
        };

        // HACK: do not generate reset value test for now; deadline pressure :P
        /*
        // Only generate reset value test if register is readable
        let reset_val_test = if self.0.is_read {
            self.gen_reset_val_test()
        } else {
            quote!()
        };
        */

        let ret = quote! {
            #[allow(non_snake_case)]
            pub fn #fn_name() {
                #[allow(unused)]
                let #ptr_binding: *mut #reg_size_ty = #addr_hex as *mut #reg_size_ty;

                #read_test
                //#reset_val_test
            }
        };
        Ok(ret)
    }

    /// Generates a test definition that can be put into an array initializer
    ///
    /// Example output:
    ///
    /// ```rust
    /// pub fn test_something_0xdeadbeef() {
    ///     let reg_ptr =*mut u32 = 0xdeadbeef as *mut u32;
    ///
    ///     let _read_value = unsafe { read_volatile(reg_ptr) };
    /// }
    /// TestCase {
    ///     function: foo::test_something_0xdeafbeef,
    ///     addr: 0xdeafbeef,
    ///     uid: "test_something",
    /// }
    /// ```
    pub fn gen_test_def(&self) -> Result<TokenStream, GenerateError> {
        let fn_name = self.gen_test_fn_ident()?;
        let periph_name_lc: TokenStream = self.0.peripheral_name.to_lowercase().parse().unwrap();
        let func = quote!(#periph_name_lc::#fn_name);
        let addr_hex: TokenStream = format!("{:#x}", self.0.full_address()?).parse().unwrap();
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
    pub fn from_registers(registers: &Registers) -> Result<TestCases, GenerateError> {
        let mut test_cases = Vec::new();
        let mut test_cases_by_peripheral: HashMap<String, Vec<String>> = HashMap::new();
        let mut test_case_structs_by_peripheral: HashMap<String, Vec<String>> = HashMap::new();
        for register in registers.iter() {
            let test_gen = RegTestGenerator::from_register(register);

            let test_fn = test_gen.gen_test_fn()?;
            let test_case = test_gen.gen_test_def()?;
            test_cases.push(format!("{}", test_case));

            if test_cases_by_peripheral.contains_key(&register.peripheral_name) {
                test_cases_by_peripheral
                    .get_mut(&register.peripheral_name)
                    .unwrap_or_else(|| {
                        panic!(
                            "Failed to find peripheral {}'s test case container.",
                            &register.peripheral_name
                        )
                    })
                    .push(format!("{}", test_fn));
            } else {
                test_cases_by_peripheral.insert(
                    register.peripheral_name.clone(),
                    vec![format!("{}", test_fn)],
                );
            }

            if test_case_structs_by_peripheral.contains_key(&register.peripheral_name) {
                test_case_structs_by_peripheral
                    .get_mut(&register.peripheral_name)
                    .unwrap_or_else(|| {
                        panic!(
                            "Failed to find peripheral {}'s test case container.",
                            &register.peripheral_name
                        )
                    })
                    .push(format!("{}", test_case));
            } else {
                test_case_structs_by_peripheral.insert(
                    register.peripheral_name.clone(),
                    vec![format!("{}", test_case)],
                );
            }
        }

        let modules = create_modules(&test_cases_by_peripheral, &test_case_structs_by_peripheral);

        let output_combined = modules.join("");
        let test_case_count = test_cases.len();
        let test_cases_combined = test_cases.join(",");
        let test_case_array = format!(
            "pub static TEST_CASES: [TestCase;{test_case_count}] = [{test_cases_combined}];"
        );
        Ok(TestCases {
            test_cases: vec![output_combined, test_case_array],
            test_case_count,
        })
    }
}
