// TODO: add structs and usages to lib.rs, no necessary to write in generator

use json::JsonValue;
use register_selftest_generator_common::{
    force_path_existence, get_environment_variable, maybe_get_environment_variable,
    validate_path_existence, Register,
};
use register_selftest_generator_parse;
use std::{
    ffi::OsString,
    fs::{self, read_to_string, File},
    io::{self, Read, Write},
    path::{Path, PathBuf},
    process::{Command, Stdio},
    str::FromStr,
};

struct TestCases {
    test_cases: Vec<String>,
    test_case_count: usize,
}

/// Extract path to output file from environment variable.
fn get_path_to_output() -> PathBuf {
    let path_str = if let Some(path_str) = maybe_get_environment_variable("OUT_DIR") {
        println!("cargo:warning=Because OUT_DIR exists, generator output is written there.");
        format!("{}/register_selftest.rs", path_str)
    } else {
        get_environment_variable("PATH_OUTPUT")
    };
    force_path_existence(&path_str)
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
        println!("cargo:warning=Because OUT_DIR exists, generator input is read from there.");
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
    let mut output = Vec::new();
    let mut test_cases = Vec::new();

    //let mut function_names = Vec::new();
    for register in registers {
        let function_name = format!(
            "test_{}_{}",
            register.name_register,
            register.full_address()
        );
        let mut statements = vec![format!(
            "#[allow(unused)] let address: *mut u32 = {} as *mut u32;",
            register.full_address()
        )];
        if register.can_read {
            statements.push("let _ = unsafe { read_volatile(address) };".to_owned());
        }
        if register.can_write {
            statements.push(format!("let reset_value = {};", register.value_reset));
            statements.push("unsafe { write_volatile(address, reset_value) };".to_owned());
        }
        let statements_combined = statements.join("");
        let statements_combined_with_result = format!("{} 0", statements_combined);
        let line = format!(
            "#[allow(non_snake_case)] pub fn {}() -> u32 {{{}}}\n",
            function_name, statements_combined_with_result
        );
        output.push(line);
        //function_names.push(function_name);
        let test_case = format!(
            "TestCase {{ name: {}, function: {}, addr: {}, uid: {} }}",
            format!("\"{}\"", register.name_register),
            function_name,
            register.full_address(),
            format!("\"{}\"", register.uid()),
        );

        test_cases.push(test_case);
    }
    let output_combined = output.join("");
    let test_case_count = test_cases.len();
    //let function_count = function_names.len();
    //let function_names_combined = function_names.join(",");
    let test_cases_combined = test_cases.join(",");
    let function_array = format!(
        "pub static TEST_CASES: [TestCase;{}] = [{}];",
        //function_count, //function_names_combined
        test_case_count,
        test_cases_combined
    );
    TestCases {
        test_cases: vec![
            "use core::ptr::read_volatile;\n".to_owned(),
            "use core::ptr::write_volatile;\n".to_owned(),
            "pub struct TestCase<'a> { pub name: &'a str, pub function: fn() -> u32, pub addr: usize, pub uid: &'a str }".to_owned(),
            output_combined,
            function_array,
        ],
        test_case_count: test_case_count, //function_count,
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
    register_selftest_generator_parse::parse();
    let mut file_output = get_output_file();
    let registers = get_registers();
    let output = create_test_cases(&registers);
    write_output(&output.test_cases, &mut file_output);
    let path = get_path_to_output();
    rustfmt_file(path).unwrap();
    println!("Wrote {} test cases.", output.test_case_count);
}
