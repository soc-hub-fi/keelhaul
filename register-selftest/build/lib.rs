//! Memory-mapped I/O peripheral register test case generator.

mod logger;

use fs_err::{self as fs, read_to_string, File};
use log::LevelFilter;
use register_test_generator::{validate_path_existence, JsonParseError, Registers, TestCases};
use std::{
    env,
    io::{self, Write},
    path::{Path, PathBuf},
    process::Command,
};

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
    let input_path = format!("{out_dir}/{}", register_test_generator::PARSED_FILENAME);
    validate_path_existence(&input_path)
}

/// Get register objects.
fn get_registers() -> Result<Registers, JsonParseError> {
    let input_json = get_input_json();
    let json_content = read_to_string(input_json).expect("Failed to read parser results.");
    let parsed_json = json::parse(&json_content).expect("Failed to parse parser results.");
    Registers::try_from(parsed_json)
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

    register_test_generator::parse();
    let mut file_output = get_output_file();
    let registers = get_registers().unwrap();
    let test_cases = TestCases::from_registers(&registers).unwrap();
    write_output(&test_cases.test_cases, &mut file_output);
    let path = get_path_to_output();
    rustfmt_file(&path)
        .unwrap_or_else(|error| panic!("Failed to format file {}. {}", path.display(), error));
    println!("Wrote {} test cases.", test_cases.test_case_count);
}
