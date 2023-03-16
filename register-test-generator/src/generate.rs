//! Generate test cases from model::* types
use crate::{GenerateError, Registers};
use log::warn;
use proc_macro2::TokenStream;
use quote::quote;
use std::collections::HashMap;

/// Place test cases in modules.
fn create_modules(
    test_cases_per_peripheral: &HashMap<String, Vec<String>>,
    test_case_structs_per_peripheral: &HashMap<String, Vec<String>>,
) -> Vec<String> {
    let mut modules = Vec::new();
    for (name_peripheral, test_cases) in test_cases_per_peripheral {
        let test_cases_catenated: TokenStream = test_cases.join("").parse().unwrap();
        let module_test_cases_combined: TokenStream = test_case_structs_per_peripheral
            .get(name_peripheral)
            .unwrap()
            .join(",")
            .parse()
            .unwrap();
        let len = test_cases.len();
        let value: TokenStream = quote!( [#module_test_cases_combined] );
        let module_test_case_array = quote! {
            pub static TEST_CASES: [TestCase; #len] = #value;
        };
        let mod_name: TokenStream = name_peripheral.to_lowercase().parse().unwrap();
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

impl TestCases {
    /// Generate test cases for each register.
    pub fn from_registers(registers: &Registers) -> Result<TestCases, GenerateError> {
        let mut test_cases = Vec::new();
        let mut test_cases_per_peripheral: HashMap<String, Vec<String>> = HashMap::new();
        let mut test_case_structs_per_peripheral: HashMap<String, Vec<String>> = HashMap::new();
        for register in registers.iter() {
            let variable_type = match register.size {
                8 => "u8",
                16 => "u16",
                32 => "u32",
                64 => "u64",
                other => {
                    warn!(
                        "Invalid register size: {other}, skipping {}",
                        register.full_path("-")
                    );
                    continue;
                }
            };
            let mut statements = vec![format!(
                "#[allow(unused)] let address: *mut {} = {:#x} as *mut {};",
                variable_type,
                register.full_address()?,
                variable_type,
            )];
            if register.is_read {
                statements.push("let _ = unsafe { read_volatile(address) };".to_owned());
            }
            if register.is_write {
                statements.push(format!(
                    "#[allow(unused)] let reset_value: {} = {};",
                    variable_type, register.reset_val
                ));
                //statements.push("unsafe { write_volatile(address, reset_value) };".to_owned());
            }
            let function_name =
                format!("test_{}_{:#x}", register.reg_name, register.full_address()?);
            let statements_combined = statements.join("");
            let line = format!(
                "#[allow(non_snake_case)] pub fn {function_name}() {{{statements_combined}}}\n"
            );
            let uid = format!("\"{}\"", register.uid());

            let function = format!(
                "{}::{}",
                register.peripheral_name.to_lowercase(),
                function_name
            );
            let test_case = format!(
                "TestCase {{ function: {}, addr: {:#x}, uid: {} }}",
                function,
                register.full_address()?,
                uid,
            );
            test_cases.push(test_case.clone());

            if test_cases_per_peripheral.contains_key(&register.peripheral_name) {
                test_cases_per_peripheral
                    .get_mut(&register.peripheral_name)
                    .unwrap_or_else(|| {
                        panic!(
                            "Failed to find peripheral {}'s test case container.",
                            &register.peripheral_name
                        )
                    })
                    .push(line);
            } else {
                test_cases_per_peripheral.insert(register.peripheral_name.clone(), vec![line]);
            }

            if test_case_structs_per_peripheral.contains_key(&register.peripheral_name) {
                test_case_structs_per_peripheral
                    .get_mut(&register.peripheral_name)
                    .unwrap_or_else(|| {
                        panic!(
                            "Failed to find peripheral {}'s test case container.",
                            &register.peripheral_name
                        )
                    })
                    .push(test_case.clone());
            } else {
                test_case_structs_per_peripheral
                    .insert(register.peripheral_name.clone(), vec![test_case]);
            }
        }

        let modules = create_modules(
            &test_cases_per_peripheral,
            &test_case_structs_per_peripheral,
        );

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
