//! Memory-mapped I/O peripheral register test case generator.

mod logger;

use std::{
    collections::HashSet,
    env,
    io::{self, Write},
    path,
    path::{Path, PathBuf},
    process::Command,
    str::FromStr,
};

use anyhow::{Context, Error};
use fs_err::{self as fs, File};
use itertools::Itertools;
use keelhaul::{
    error::SvdParseError, parse_architecture_size, ArchPtr, Filters, ItemFilter, ModelSource,
    ParseTestKindError, PtrSize, RegTestKind, Registers, TestCases, TestConfig,
};
use log::{info, LevelFilter};
use regex::Regex;

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

/// Returns contents of a file at `path`, panicking on any failure
///
/// # Panics
///
/// This function panics if the path does not exist, or if the file cannot be
/// read.
#[must_use]
pub fn read_file_or_panic(path: &path::Path) -> String {
    path.canonicalize().map_or_else(
        |err| panic!("path {} does not exist: {err}", path.display()),
        |p| {
            fs::read_to_string(p)
                .unwrap_or_else(|err| panic!("cannot read file at path {}: {err}", path.display()))
        },
    )
}

/// Try to extract path to excludes-file from environment variable.
fn read_file_from_env_or_panic(var: &str) -> Option<String> {
    env::var(var)
        .ok()
        .map(|p| read_file_or_panic(&PathBuf::from(p)))
}

/// Returns a vector containing elements read from environment variable `var` if
/// the variable is present.
///
/// # Parameters:
///
/// `var` - The name of the environment variable to be read
/// `sep` - The separator for Vec elements
fn read_vec_from_env(var: &str, sep: char) -> Option<Vec<String>> {
    env::var(var)
        .map(|s| {
            let peripherals = s.split(sep).map(ToOwned::to_owned).collect_vec();
            // TODO: verify that these are valid peripherals
            peripherals
        })
        .ok()
}

/// Try to get names of excluded registers.
fn read_excludes_from_env() -> Option<Vec<String>> {
    read_file_from_env_or_panic("PATH_EXCLUDES").map(|contents|
            // One register per line
            contents.split('\n').map(ToOwned::to_owned).collect_vec())
}

/// Parse SVD-file.
///
/// # Panics
///
/// - Missing path to SVD-file
///
/// # Errors
///
/// - Failed to interpret given options
/// - Failed to parse given SVD file
pub fn parse<P: ArchPtr>(svd_path: impl AsRef<Path>) -> Result<Registers<P>, Error>
where
    SvdParseError: From<<P as num::Num>::FromStrRadixErr>
        + From<<P as FromStr>::Err>
        + From<<P as TryFrom<u64>>::Error>,
    <P as TryFrom<u64>>::Error: std::fmt::Debug,
{
    let include_peripherals = read_vec_from_env("INCLUDE_PERIPHERALS", ',');
    let exclude_peripherals = read_vec_from_env("EXCLUDE_PERIPHERALS", ',');
    let periph_filter =
        ItemFilter::list(include_peripherals, exclude_peripherals.unwrap_or_default());
    let include_syms_regex = env::var("INCLUDE_SYMS_REGEX")
        .ok()
        .map(|s| Regex::new(&s))
        .transpose()?;
    let exclude_syms_regex = env::var("EXCLUDE_SYMS_REGEX")
        .ok()
        .map(|s| Regex::new(&s))
        .transpose()?;
    let syms_filter = ItemFilter::regex(include_syms_regex, exclude_syms_regex);
    let reg_filter = ItemFilter::list(None, read_excludes_from_env().unwrap_or_default());
    Ok(keelhaul::parse_registers(
        &[ModelSource::new(
            svd_path.as_ref().to_path_buf(),
            keelhaul::SourceFormat::Svd,
        )],
        Filters::from_filters(reg_filter, periph_filter, syms_filter),
    )?
    .into_iter()
    .nth(0)
    .unwrap())
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
            let registers = parse::<u8>(&svd_path)?;
            TestCases::from_registers(&registers, &test_cfg)
        }
        PtrSize::U16 => {
            let registers = parse::<u16>(&svd_path)?;
            TestCases::from_registers(&registers, &test_cfg)
        }
        PtrSize::U32 => {
            let registers = parse::<u32>(&svd_path)?;
            TestCases::from_registers(&registers, &test_cfg)
        }
        PtrSize::U64 => {
            let registers = parse::<u64>(&svd_path)?;
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
