// TODO: add structs and usages to lib.rs, no necessary to write in generator

use json::JsonValue;
use register_selftest_generator_common::{
    get_environment_variable, maybe_get_environment_variable, validate_path_existence, Register,
};
use std::{
    collections::HashMap,
    env,
    fs::{self, read_to_string, File},
    io::{self, Write},
    path::{Path, PathBuf},
    process::Command,
};

struct TestCases {
    test_cases: Vec<String>,
    test_case_count: usize,
}

/// Extract path to output file from environment variable.
fn get_path_to_output() -> PathBuf {
    // Safety: OUT_DIR always exists at build time
    let out_dir = env::var("OUT_DIR").unwrap();
    let out_dir = PathBuf::from(out_dir);
    out_dir.join("register_selftest.rs")
}

/// Get handle to output file.
fn get_output_file() -> File {
    let path = get_path_to_output();
    fs::OpenOptions::new()
        .create(true)
        .write(true)
        .truncate(true)
        .open(path)
        .expect("Failed to open output file.")
}

fn get_input_path() -> PathBuf {
    let path_str = if let Some(path_str) = maybe_get_environment_variable("OUT_DIR") {
        format!("{}/parsed.json", path_str)
    } else {
        get_environment_variable("PATH_JSON")
    };
    validate_path_existence(&path_str)
}

/// Extract registers from JSON object.
fn get_parsed_registers(content: JsonValue) -> Vec<Register> {
    let mut registers = Vec::new();
    match content {
        JsonValue::Array(array) => {
            for value in array {
                let register = match value {
                    JsonValue::Object(object) => Register {
                        name_peripheral: object
                            .get("name_peripheral")
                            .expect("JSON object does not contain 'name_peripheral'-field.")
                            .to_string(),
                        name_cluster: object
                            .get("name_cluster")
                            .expect("JSON object does not contain 'name_cluster'-field.")
                            .to_string(),
                        name_register: object
                            .get("name_register")
                            .expect("JSON object does not contain 'name_register'-field.")
                            .to_string(),
                        address_base: object
                            .get("address_base")
                            .expect("JSON object does not contain 'address_base'-field.")
                            .to_string()
                            .parse()
                            .expect("Failed to parse 'address_base'-field as integer."),
                        address_offset_cluster: object
                            .get("address_offset_cluster")
                            .expect("JSON object does not contain 'address_offset_cluster'-field.")
                            .to_string()
                            .parse()
                            .expect("Failed to parse 'address_offset_cluster'-field as integer."),
                        address_offset_register: object
                            .get("address_offset_register")
                            .expect("JSON object does not contain 'address_offset_register'-field.")
                            .to_string()
                            .parse()
                            .expect("Failed to parse 'address_offset_register'-field as integer."),
                        value_reset: object
                            .get("value_reset")
                            .expect("JSON object does not contain 'value_reset'-field.")
                            .to_string()
                            .parse()
                            .expect("Failed to parse 'value_reset'-field as integer."),
                        can_read: object
                            .get("can_read")
                            .expect("JSON object does not contain 'can_read'-field.")
                            .to_string()
                            .parse()
                            .expect("Failed to parse 'can_read'-field as integer."),
                        can_write: object
                            .get("can_write")
                            .expect("JSON object does not contain 'can_write'-field.")
                            .to_string()
                            .parse()
                            .expect("Failed to parse 'can_write'-field as integer."),
                        size: object
                            .get("size")
                            .expect("JSON object does not contain 'size'-field.")
                            .to_string()
                            .parse()
                            .expect("Failed to parse 'size'-field as integer."),
                    },
                    _ => panic!("Illegal JSON object type."),
                };
                registers.push(register);
            }
        }
        _ => panic!("Illegal JSON object type."),
    }
    registers
}

/// Get register objects.
fn get_registers() -> Vec<Register> {
    let path_input = get_input_path();
    let content = read_to_string(path_input).expect("Failed to read parser results.");
    let json = json::parse(&content).expect("Failed to parse parser results.");
    get_parsed_registers(json)
}

/// Generate test cases for each register.
fn create_test_cases(registers: &Vec<Register>) -> TestCases {
    let mut test_cases = Vec::new();
    let mut test_cases_per_peripheral: HashMap<String, Vec<String>> = HashMap::new();
    let mut test_case_structs_per_peripheral: HashMap<String, Vec<String>> = HashMap::new();
    for register in registers {
        let variable_type = match register.size {
            8 => "u8",
            16 => "u16",
            32 => "u32",
            64 => "u64",
            other => panic!("Invalid register size: {}", other),
        };
        let function_name = format!(
            "test_{}_{}",
            register.name_register,
            register.full_address()
        );
        let mut statements = vec![format!(
            "#[allow(unused)] let address: *mut {} = {} as *mut {};",
            variable_type,
            register.full_address(),
            variable_type,
        )];
        if register.can_read {
            statements.push("let _ = unsafe { read_volatile(address) };".to_owned());
        }
        if register.can_write {
            statements.push(format!(
                "#[allow(unused)] let reset_value: {} = {};",
                variable_type, register.value_reset
            ));
            //statements.push("unsafe { write_volatile(address, reset_value) };".to_owned());
        }
        let statements_combined = statements.join("");
        let result_type = match register.size {
            8 => "FuncRet::U8",
            16 => "FuncRet::U16",
            32 => "FuncRet::U32",
            64 => "FuncRet::U64",
            other => panic!("Invalid register size: {}", other),
        };
        //let statements_combined_with_result = format!("{} {}(0)", statements_combined, result_type);
        let line = format!(
            "#[allow(non_snake_case)] pub fn {}() {{{}}}\n",
            //function_name, statements_combined_with_result
            function_name,
            statements_combined
        );
        let uid = format!("\"{}\"", register.uid());

        let function = format!(
            "{}::{}",
            register.name_peripheral.to_lowercase(),
            function_name
        );
        let test_case = format!(
            "TestCase {{ function: {}, addr: {}, uid: {} }}",
            function,
            register.full_address(),
            uid,
        );
        test_cases.push(test_case.clone());

        if test_cases_per_peripheral.contains_key(&register.name_peripheral) {
            test_cases_per_peripheral
                .get_mut(&register.name_peripheral)
                .unwrap()
                .push(line);
        } else {
            test_cases_per_peripheral.insert(register.name_peripheral.clone(), vec![line]);
        }

        if test_case_structs_per_peripheral.contains_key(&register.name_peripheral) {
            test_case_structs_per_peripheral
                .get_mut(&register.name_peripheral)
                .unwrap()
                .push(test_case.clone());
        } else {
            test_case_structs_per_peripheral
                .insert(register.name_peripheral.clone(), vec![test_case]);
        }
    }

    let mut modules = Vec::new();
    for (name_peripheral, test_cases) in test_cases_per_peripheral {
        let test_cases_combined = test_cases.join("");
        let module_test_case_count = test_cases.len();
        let module_test_cases_combined = test_case_structs_per_peripheral
            .get(&name_peripheral)
            .unwrap()
            .join(",");
        let module_test_case_array = format!(
            "pub static TEST_CASES: [TestCase;{}] = [{}];",
            module_test_case_count, module_test_cases_combined
        );
        let module = format!(
            "pub mod {} {{ use super::*; {} {} }}",
            name_peripheral.to_lowercase(),
            test_cases_combined,
            module_test_case_array,
        );
        modules.push(module);
    }

    let output_combined = modules.join("");
    let test_case_count = test_cases.len();
    let test_cases_combined = test_cases.join(",");
    let test_case_array = format!(
        "pub static TEST_CASES: [TestCase;{}] = [{}];",
        test_case_count, test_cases_combined
    );
    TestCases {
        test_cases: vec![output_combined, test_case_array],
        test_case_count: test_case_count,
    }
}

/// Write test cases to output file.
fn write_output(lines: &Vec<String>, file: &mut File) {
    for line in lines {
        file.write_all(line.as_bytes())
            .expect("Failed to write to output file.");
    }
}

fn rustfmt_file(f: impl AsRef<Path>) -> io::Result<()> {
    let f = f.as_ref();
    run_cmd("rustfmt", &[format!("{}", f.display())])?;
    Ok(())
}

fn run_cmd(cmd: &str, params: &[impl AsRef<str>]) -> io::Result<()> {
    let mut cmd = &mut Command::new(cmd);
    for param in params {
        cmd = cmd.arg(param.as_ref());
    }
    cmd.spawn()?;
    Ok(())
}

pub fn main() {
    println!("cargo:rerun-if-env-changed=INCLUDE_PERIPHERALS");
    println!("cargo:rerun-if-env-changed=EXCLUDE_PERIPHERALS");
    println!("cargo:rerun-if-env-changed=PATH_SVD");
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=src");
    println!("cargo:rerun-if-changed=../register-selftest-generator-common");
    println!("cargo:rerun-if-changed=../register-selftest-generator-parse");
    println!("cargo:rerun-if-changed=../register-selftest-generator-runner");
    register_selftest_generator_parse::parse();
    let mut file_output = get_output_file();
    let registers = get_registers();
    let output = create_test_cases(&registers);
    write_output(&output.test_cases, &mut file_output);
    let path = get_path_to_output();
    rustfmt_file(path).unwrap();
    println!("Wrote {} test cases.", output.test_case_count);
}
