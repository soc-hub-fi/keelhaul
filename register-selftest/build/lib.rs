//! Memory-mapped I/O peripheral register test case generator.

mod logger;

use fs_err::{self as fs, File};
use log::LevelFilter;
use register_test_generator::{
    FailureImplKind, ParseTestKindError, RegTestKind, TestCases, TestConfig,
};
use std::{
    collections::HashSet,
    env,
    io::{self, Write},
    path::{Path, PathBuf},
    process::Command,
    str::FromStr,
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

fn test_types_from_env() -> Result<Option<HashSet<RegTestKind>>, ParseTestKindError> {
    match env::var("INCLUDE_TEST_KINDS") {
        Ok(inc_tests_var) => Some(
            inc_tests_var
                .split(',')
                .map(RegTestKind::from_str)
                .collect(),
        ),
        Err(_) => None,
    }
    .transpose()
}

pub fn main() -> anyhow::Result<()> {
    println!("cargo:rerun-if-env-changed=INCLUDE_PERIPHERALS");
    println!("cargo:rerun-if-env-changed=EXCLUDE_PERIPHERALS");
    println!("cargo:rerun-if-env-changed=INCLUDE_SYMS_REGEX");
    println!("cargo:rerun-if-env-changed=EXCLUDE_SYMS_REGEX");
    println!("cargo:rerun-if-env-changed=INCLUDE_TEST_KINDS");
    println!("cargo:rerun-if-env-changed=PATH_SVD");
    println!("cargo:rerun-if-env-changed=SVD_PATH");
    println!("cargo:rerun-if-changed=build.rs");

    // Install a logger to print useful messages into `cargo:warning={}`
    logger::init(LevelFilter::Info);

    let mut file_output = get_output_file();
    let registers = register_test_generator::parse()?;

    let mut test_cfg = TestConfig::default();
    if let Some(test_kind_set) = test_types_from_env()? {
        // HACK: use panicking tests with scan for Headsail convenience, even if not requested
        if test_kind_set.contains(&RegTestKind::ReadIsResetVal) {
            test_cfg = test_cfg.on_fail(FailureImplKind::Panic)?;
        }
        test_cfg = test_cfg.reg_test_kinds(test_kind_set)?;
    }
    let test_cases = TestCases::from_registers(&registers, &test_cfg).unwrap();
    file_output.write_all(test_cases.to_module_string().as_bytes())?;
    let path = get_path_to_output();
    rustfmt_file(&path)
        .unwrap_or_else(|error| panic!("Failed to format file {}. {}", path.display(), error));
    println!("Wrote {} test cases.", test_cases.test_case_count);
    Ok(())
}
