//! Memory-mapped I/O peripheral register test case generator.

mod logger;

use fs_err::{self as fs, read_to_string, File};
use log::{warn, LevelFilter};
use register_selftest_generator_common::{validate_path_existence, RegisterParseError, Registers};
use std::{
    collections::HashMap,
    env,
    io::{self, Write},
    path::{Path, PathBuf},
    process::Command,
};

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

/// Get register objects.
fn get_registers() -> Result<Registers, RegisterParseError> {
    let input_json = get_input_json();
    let json_content = read_to_string(input_json).expect("Failed to read parser results.");
    let parsed_json = json::parse(&json_content).expect("Failed to parse parser results.");
    Registers::try_from(parsed_json)
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
fn create_test_cases(registers: &Registers) -> TestCases {
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
                    register.reg_name
                );
                continue;
            }
        };
        let function_name = format!("test_{}_{:#x}", register.reg_name, register.full_address());
        let mut statements = vec![format!(
            "#[allow(unused)] let address: *mut {} = {:#x} as *mut {};",
            variable_type,
            register.full_address(),
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
            register.full_address(),
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

    // Install a logger to print useful messages into `cargo:warning={}`
    logger::init(LevelFilter::Info);

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
