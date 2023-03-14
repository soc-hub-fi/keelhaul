//! Memory-mapped I/O peripheral register test case generator.

use fs_err::{self as fs, read_to_string, File};
use json::JsonValue;
use log::warn;
use register_selftest_generator_common::{validate_path_existence, Register};
use std::{
    collections::HashMap,
    env,
    io::{self, Write},
    num::ParseIntError,
    path::{Path, PathBuf},
    process::Command,
    str::ParseBoolError,
};
use thiserror::Error;

/// Collection of all test cases for this build.
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

/// Get path to parser output.
fn get_input_json() -> PathBuf {
    // Safety: OUT_DIR always exists
    let out_dir = env::var("OUT_DIR").unwrap();
    let input_path = format!(
        "{out_dir}/{}",
        register_selftest_generator_parse::PARSED_FILENAME
    );
    validate_path_existence(&input_path)
}

#[derive(Error, Debug)]
enum RegisterParseError {
    #[error("expected JSON object: {0}")]
    ExpectedJsonObject(String),
    #[error("expected JSON array: {0}")]
    ExpectedJsonArray(String),
    #[error("JSON object does not contain field for '{0}'")]
    FieldNotFound(String),
    #[error("could not parse int")]
    ParseInt(#[from] ParseIntError),
    #[error("could not parse bool")]
    ParseBool(#[from] ParseBoolError),
}

fn json_object_to_register(object: &json::object::Object) -> Result<Register, RegisterParseError> {
    let get_field =
        |obj: &json::object::Object, field: &str| -> Result<String, RegisterParseError> {
            obj.get(field)
                .ok_or(RegisterParseError::FieldNotFound(field.to_owned()))
                .map(|x| x.to_string())
        };
    let name_peripheral = get_field(object, "name_peripheral")?;
    let name_cluster = get_field(object, "name_cluster")?;
    let name_register = get_field(object, "name_register")?;
    let address_base = get_field(object, "address_base")?.parse()?;
    let address_offset_cluster = get_field(object, "address_offset_cluster")?.parse()?;
    let address_offset_register = get_field(object, "address_offset_register")?.parse()?;
    let value_reset = get_field(object, "value_reset")?.parse()?;
    let can_read = get_field(object, "can_read")?.parse()?;
    let can_write = get_field(object, "can_write")?.parse()?;
    let size = get_field(object, "size")?.parse()?;
    Ok(Register {
        name_peripheral,
        name_cluster,
        name_register,
        address_base,
        address_offset_cluster,
        address_offset_register,
        value_reset,
        can_read,
        can_write,
        size,
    })
}

/// Extract registers from JSON object.
fn json_value_into_registers(content: JsonValue) -> Result<Vec<Register>, RegisterParseError> {
    match content {
        JsonValue::Array(array) => array
            .iter()
            .map(|value| match value {
                JsonValue::Object(object) => json_object_to_register(object),
                _ => Err(RegisterParseError::ExpectedJsonObject(format!("{value:?}"))),
            })
            .collect(),
        _ => Err(RegisterParseError::ExpectedJsonArray(format!(
            "{content:?}"
        ))),
    }
}

/// Get register objects.
fn get_registers() -> Result<Vec<Register>, RegisterParseError> {
    let input_json = get_input_json();
    let json_content = read_to_string(input_json).expect("Failed to read parser results.");
    let parsed_json = json::parse(&json_content).expect("Failed to parse parser results.");
    json_value_into_registers(parsed_json)
}

/// # Arguments
///
/// `name_uc`   - Uppercase name for the array
/// `elem_type` - Type for the array elements
/// `len`       - Length for the array
/// `value`     - Value for the array ("... = {value};")
fn gen_static_array_str(name_uc: &str, elem_type: &str, len: usize, value: &str) -> String {
    format!("pub static {name_uc}: [{elem_type}; {len}] = {value};")
}

/// # Arguments
///
/// `name_lc`   - Lowercase name for the module
fn gen_mod_str(name_lc: &str, contents: &str) -> String {
    format!(
        r#"
pub mod {name_lc} {{
    use super::*;

    {}
}}
"#,
        contents
    )
}

/// Place test cases in modules.
fn create_modules(
    test_cases_per_peripheral: &HashMap<String, Vec<String>>,
    test_case_structs_per_peripheral: &HashMap<String, Vec<String>>,
) -> Vec<String> {
    let mut modules = Vec::new();
    for (name_peripheral, test_cases) in test_cases_per_peripheral {
        let test_cases_catenated = test_cases.join("");
        let module_test_cases_combined = test_case_structs_per_peripheral
            .get(name_peripheral)
            .unwrap()
            .join(",");
        let module_test_case_array = gen_static_array_str(
            "TEST_CASES",
            "TestCase",
            test_cases.len(),
            &format!("[{}]", module_test_cases_combined),
        );
        let module = gen_mod_str(
            &name_peripheral.to_lowercase(),
            &format!("{} {}", module_test_case_array, test_cases_catenated),
        );
        modules.push(module);
    }
    modules
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
            other => panic!("Invalid register size: {other}"),
        };
        let function_name = format!(
            "test_{}_{:#x}",
            register.name_register,
            register.full_address()
        );
        let mut statements = vec![format!(
            "#[allow(unused)] let address: *mut {} = {:#x} as *mut {};",
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
        let line = format!(
            "#[allow(non_snake_case)] pub fn {function_name}() {{{statements_combined}}}\n"
        );
        let uid = format!("\"{}\"", register.uid());

        let function = format!(
            "{}::{}",
            register.name_peripheral.to_lowercase(),
            function_name
        );
        let test_case = format!(
            "TestCase {{ function: {}, addr: {:#x}, uid: {} }}",
            function,
            register.full_address(),
            uid,
        );
        test_cases.push(test_case.clone());

        if test_cases_per_peripheral.contains_key(&register.name_peripheral) {
            test_cases_per_peripheral
                .get_mut(&register.name_peripheral)
                .unwrap_or_else(|| {
                    panic!(
                        "Failed to find peripheral {}'s test case container.",
                        &register.name_peripheral
                    )
                })
                .push(line);
        } else {
            test_cases_per_peripheral.insert(register.name_peripheral.clone(), vec![line]);
        }

        if test_case_structs_per_peripheral.contains_key(&register.name_peripheral) {
            test_case_structs_per_peripheral
                .get_mut(&register.name_peripheral)
                .unwrap_or_else(|| {
                    panic!(
                        "Failed to find peripheral {}'s test case container.",
                        &register.name_peripheral
                    )
                })
                .push(test_case.clone());
        } else {
            test_case_structs_per_peripheral
                .insert(register.name_peripheral.clone(), vec![test_case]);
        }
    }

    let modules = create_modules(
        &test_cases_per_peripheral,
        &test_case_structs_per_peripheral,
    );

    let output_combined = modules.join("");
    let test_case_count = test_cases.len();
    let test_cases_combined = test_cases.join(",");
    let test_case_array =
        format!("pub static TEST_CASES: [TestCase;{test_case_count}] = [{test_cases_combined}];");
    TestCases {
        test_cases: vec![output_combined, test_case_array],
        test_case_count,
    }
}

/// Write test cases to output file.
fn write_output(lines: &Vec<String>, file: &mut File) {
    for line in lines {
        file.write_all(line.as_bytes())
            .expect("Failed to write to output file.");
    }
}

/// Execute shell command.
fn run_cmd(cmd: &str, params: &[impl AsRef<str>]) -> io::Result<()> {
    let mut cmd = &mut Command::new(cmd);
    for param in params {
        cmd = cmd.arg(param.as_ref());
    }
    cmd.spawn()?;
    Ok(())
}

/// Format file in place.
fn rustfmt_file(path: impl AsRef<Path>) -> io::Result<()> {
    run_cmd("rustfmt", &[format!("{}", path.as_ref().display())])?;
    Ok(())
}

pub fn main() {
    println!("cargo:rerun-if-env-changed=INCLUDE_PERIPHERALS");
    println!("cargo:rerun-if-env-changed=EXCLUDE_PERIPHERALS");
    println!("cargo:rerun-if-env-changed=PATH_SVD");
    println!("cargo:rerun-if-changed=build.rs");

    register_selftest_generator_parse::parse();
    let mut file_output = get_output_file();
    let registers = get_registers().unwrap();
    let test_cases = create_test_cases(&registers);
    write_output(&test_cases.test_cases, &mut file_output);
    let path = get_path_to_output();
    rustfmt_file(&path)
        .unwrap_or_else(|error| panic!("Failed to format file {}. {}", path.display(), error));
    println!("Wrote {} test cases.", test_cases.test_case_count);
}
