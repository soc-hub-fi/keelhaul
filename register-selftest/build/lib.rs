//! Memory-mapped I/O peripheral register test case generator.

mod logger;

use anyhow::{Context, Error};
use fs_err::{self as fs, File};
use keelhaul::{
    parse_architecture_size, ParseTestKindError, PtrSize, RegTestKind, TestCases, TestConfig,
};
use log::{info, LevelFilter};
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
    let out_dir = env::var("OUTPUT_PATH").map_or_else(
        |_err| {
            // Safety: OUT_DIR always exists at build time
            let out_dir = env::var("OUT_DIR").unwrap();
            PathBuf::from(out_dir)
        },
        PathBuf::from,
    );
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
    env::var("INCLUDE_TEST_KINDS")
        .map_or_else(
            |_err| None,
            |included_tests| {
                Some(
                    included_tests
                        .split(',')
                        .map(RegTestKind::from_str)
                        .collect(),
                )
            },
        )
        .transpose()
}

fn arch_ptr_size_from_env() -> anyhow::Result<Option<PtrSize>> {
    if let Ok(bytes_str) = env::var("ARCH_PTR_BYTES") {
        let ptr_size = bytes_str
            .parse::<u8>()
            .with_context(|| "ARCH_PTR_BYTES")
            .unwrap()
            .try_into()?;
        info!("Pointer size is overriden with: {:?}", ptr_size);
        Ok(Some(ptr_size))
    } else {
        Ok(None)
    }
}

fn solve_architecture_size() -> Result<PtrSize, Error> {
    match arch_ptr_size_from_env()? {
        Some(size) => Ok(size),
        None => {
            let svd_path = read_path_from_env("SVD_PATH").unwrap();
            // Parse size from SVD-file.
            Ok(parse_architecture_size(svd_path)?)
        }
    }
}

fn read_path_from_env(var: &str) -> Result<PathBuf, Error> {
    let svd_path = env::var(var)?;
    Ok(env::current_dir()
        .expect("cannot access current working dir")
        .join(svd_path))
}

pub fn main() -> anyhow::Result<()> {
    println!("cargo:rerun-if-env-changed=INCLUDE_PERIPHERALS");
    println!("cargo:rerun-if-env-changed=EXCLUDE_PERIPHERALS");
    println!("cargo:rerun-if-env-changed=INCLUDE_SYMS_REGEX");
    println!("cargo:rerun-if-env-changed=EXCLUDE_SYMS_REGEX");
    println!("cargo:rerun-if-env-changed=INCLUDE_TEST_KINDS");
    println!("cargo:rerun-if-env-changed=SVD_PATH");
    // TODO: this info can be found from SVD-file, providing it via CLI is redundant, or is it?
    println!("cargo:rerun-if-env-changed=ARCH_PTR_BYTES");
    println!("cargo:rerun-if-env-changed=OUTPUT_PATH");
    println!("cargo:rerun-if-changed=build.rs");

    // Install a logger to print useful messages into `cargo:warning={}`
    logger::init(LevelFilter::Info);

    let arch_ptr_size = solve_architecture_size()?;
    let mut test_cfg = TestConfig::new(arch_ptr_size);
    if let Some(test_kind_set) = test_types_from_env()? {
        test_cfg = test_cfg.reg_test_kinds(test_kind_set)?;
    }
    let mut file_output = get_output_file();

    let svd_path = read_path_from_env("SVD_PATH")?;
    let test_cases: TestCases = match arch_ptr_size {
        PtrSize::U8 => {
            let registers = keelhaul::parse::<u8>(&svd_path)?;
            TestCases::from_registers(&registers, &test_cfg)
        }
        PtrSize::U16 => {
            let registers = keelhaul::parse::<u16>(&svd_path)?;
            TestCases::from_registers(&registers, &test_cfg)
        }
        PtrSize::U32 => {
            let registers = keelhaul::parse::<u32>(&svd_path)?;
            TestCases::from_registers(&registers, &test_cfg)
        }
        PtrSize::U64 => {
            let registers = keelhaul::parse::<u64>(&svd_path)?;
            TestCases::from_registers(&registers, &test_cfg)
        }
    }?;

    file_output.write_all(test_cases.to_module_string().as_bytes())?;
    let path = get_path_to_output();
    rustfmt_file(&path).unwrap_or_else(|error: io::Error| {
        panic!("Failed to format file {}. {}", path.display(), error)
    });
    info!("Wrote {} test cases.", test_cases.test_case_count);
    Ok(())
}
