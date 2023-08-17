//! Memory-mapped I/O peripheral register test case generator.

mod logger;

use anyhow::Context;
use fs_err::{self as fs, File};
use log::{info, warn, LevelFilter};
use register_test_generator::{
    ParseTestKindError, PtrSize, RegTestKind, Registers, TestCases, TestConfig,
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

fn arch_ptr_size_from_env() -> anyhow::Result<PtrSize> {
    if let Ok(bytes_str) = env::var("ARCH_PTR_BYTES") {
        let ptr_size = bytes_str
            .parse::<u8>()
            .with_context(|| "ARCH_PTR_BYTES")
            .unwrap()
            .try_into()?;
        info!("Selected ptr size: {:?}", ptr_size);
        Ok(ptr_size)
    } else {
        warn!("ARCH_PTR_BYTES not specified, assuming 4-byte addressable platform (32-bit)");
        Ok(PtrSize::U32)
    }
}

fn parse_registers_u8() -> anyhow::Result<Registers<u8>> {
    Ok(register_test_generator::parse::<u8>()?)
}

fn parse_registers_u16() -> anyhow::Result<Registers<u16>> {
    Ok(register_test_generator::parse::<u16>()?)
}

fn parse_registers_u32() -> anyhow::Result<Registers<u32>> {
    Ok(register_test_generator::parse::<u32>()?)
}

fn parse_registers_u64() -> anyhow::Result<Registers<u64>> {
    Ok(register_test_generator::parse::<u64>()?)
}

pub fn main() -> anyhow::Result<()> {
    println!("cargo:rerun-if-env-changed=INCLUDE_PERIPHERALS");
    println!("cargo:rerun-if-env-changed=EXCLUDE_PERIPHERALS");
    println!("cargo:rerun-if-env-changed=INCLUDE_SYMS_REGEX");
    println!("cargo:rerun-if-env-changed=EXCLUDE_SYMS_REGEX");
    println!("cargo:rerun-if-env-changed=INCLUDE_TEST_KINDS");
    // TODO: deprecate PATH_SVD
    println!("cargo:rerun-if-env-changed=PATH_SVD");
    println!("cargo:rerun-if-env-changed=SVD_PATH");
    // TODO: this info can be found from SVD-file, providing it via CLI is redundant, or is it?
    println!("cargo:rerun-if-env-changed=ARCH_PTR_BYTES");
    println!("cargo:rerun-if-env-changed=OUTPUT_PATH");
    println!("cargo:rerun-if-changed=build.rs");

    // Install a logger to print useful messages into `cargo:warning={}`
    logger::init(LevelFilter::Info);

    let arch_ptr_size = arch_ptr_size_from_env()?;
    let mut test_cfg = TestConfig::new(arch_ptr_size);
    if let Some(test_kind_set) = test_types_from_env()? {
        test_cfg = test_cfg.reg_test_kinds(test_kind_set)?;
    }

    let mut file_output = get_output_file();
    let test_cases = match arch_ptr_size {
        PtrSize::U8 => {
            let registers = parse_registers_u8()?;
            TestCases::from_registers(&registers, &test_cfg)
        }
        PtrSize::U16 => {
            let registers = parse_registers_u16()?;
            TestCases::from_registers(&registers, &test_cfg)
        }
        PtrSize::U32 => {
            let registers = parse_registers_u32()?;
            TestCases::from_registers(&registers, &test_cfg)
        }
        PtrSize::U64 => {
            let registers = parse_registers_u64()?;
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
