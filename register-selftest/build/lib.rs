//! Memory-mapped I/O peripheral register test case generator.

mod logger;
mod util;

use std::{
    collections::HashSet,
    env,
    io::{self, Write},
    path,
    path::{Path, PathBuf},
    str::FromStr,
};

use anyhow::{Context, Error};
use fs_err::{self as fs, File};
use itertools::Itertools;
use keelhaul::{
    error::SvdParseError, ArchPtr, Filters, ItemFilter, ModelSource, ParseTestKindError, PtrSize,
    RegTestKind, Registers, TestCases, TestConfig,
};
use log::{info, LevelFilter};
use regex::Regex;
use util::read_vec_from_env;

const ENV_SVD_IN: &str = "SVD_PATH";
/// `OUT_DIR` is predefined by the Rust compiler as the default output directory for all artifacts
const ENV_OUT_DIR: &str = "OUT_DIR";
/// `ENV_OUT_DIR_OVERRIDE` can be used to override the output directory (or file) for the register generator
const ENV_OUT_DIR_OVERRIDE: &str = "OUTPUT_PATH";
/// `ENV_TEST_KINDS` is used to select which tests are run
const ENV_TEST_KINDS: &str = "INCLUDE_TEST_KINDS";
const ENV_ARCH: &str = "ARCH_PTR_BYTES";
const ENV_INCLUDE_PERIPHS: &str = "INCLUDE_PERIPHERALS";
const ENV_EXCLUDE_PERIPHS: &str = "EXCLUDE_PERIPHERALS";
const ENV_INCLUDE_SYMS_REGEX: &str = "INCLUDE_SYMS_REGEX";
const ENV_EXCLUDE_SYMS_REGEX: &str = "EXCLUDE_SYMS_REGEX";

/// Extract path to final output file from environment variables
fn get_path_to_output() -> PathBuf {
    // Use `OUTPUT_PATH` or alternatively `OUT_DIR` if the former didn't exist
    let out_dir = util::read_abspath_from_env(ENV_OUT_DIR_OVERRIDE).unwrap_or_else(|_var_err|
            // Safety: OUT_DIR always exists at build time
            util::read_abspath_from_env(ENV_OUT_DIR).unwrap());
    if out_dir.is_file() {
        return out_dir;
    }

    out_dir.join("register_selftest.rs")
}

/// Open a file handle to the final output file
fn open_output_file() -> File {
    let path = get_path_to_output();
    fs::OpenOptions::new()
        .create(true)
        .write(true)
        .truncate(true)
        .open(path)
        .expect("Failed to open output file.")
}

/// Format file in place
fn rustfmt_file(path: impl AsRef<Path>) -> io::Result<()> {
    util::run_cmd("rustfmt", &[format!("{}", path.as_ref().display())])
}

fn test_types_from_env() -> Result<Option<HashSet<RegTestKind>>, ParseTestKindError> {
    let test_kinds = read_vec_from_env(ENV_TEST_KINDS, ',');
    if let Ok(test_kinds) = test_kinds {
        Ok(Some(
            test_kinds
                .into_iter()
                .map(|test_kind| RegTestKind::from_str(&test_kind))
                .collect::<Result<HashSet<_>, ParseTestKindError>>()?,
        ))
    } else {
        Ok(None)
    }
}

fn arch_ptr_size_from_env() -> anyhow::Result<PtrSize> {
    let bytes_str = env::var(ENV_ARCH)?;
    let ptr_size = bytes_str.parse::<u8>().with_context(|| ENV_ARCH).unwrap();
    Ok(ptr_size.try_into()?)
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
    let include_peripherals = util::read_vec_from_env(ENV_INCLUDE_PERIPHS, ',').ok();
    let exclude_peripherals = util::read_vec_from_env(ENV_EXCLUDE_PERIPHS, ',').ok();
    let periph_filter =
        ItemFilter::list(include_peripherals, exclude_peripherals.unwrap_or_default());
    let include_syms_regex = env::var(ENV_INCLUDE_SYMS_REGEX)
        .ok()
        .map(|s| Regex::new(&s))
        .transpose()?;
    let exclude_syms_regex = env::var(ENV_EXCLUDE_SYMS_REGEX)
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
    .next()
    .unwrap())
}

pub fn main() -> anyhow::Result<()> {
    println!("cargo:rerun-if-env-changed={ENV_INCLUDE_PERIPHS}");
    println!("cargo:rerun-if-env-changed={ENV_EXCLUDE_PERIPHS}");
    println!("cargo:rerun-if-env-changed={ENV_INCLUDE_SYMS_REGEX}");
    println!("cargo:rerun-if-env-changed={ENV_EXCLUDE_SYMS_REGEX}");
    println!("cargo:rerun-if-env-changed={ENV_TEST_KINDS}");
    println!("cargo:rerun-if-env-changed={ENV_SVD_IN}");
    println!("cargo:rerun-if-env-changed={ENV_ARCH}");
    println!("cargo:rerun-if-env-changed={ENV_OUT_DIR_OVERRIDE}");

    // Install a logger to print useful messages into `cargo:warning={}`
    logger::init(LevelFilter::Info);

    let arch_ptr_size = arch_ptr_size_from_env()?;
    let mut test_cfg = TestConfig::new(arch_ptr_size);
    if let Some(test_kind_set) = test_types_from_env()? {
        test_cfg = test_cfg.reg_test_kinds(test_kind_set)?;
    }
    let mut file_output = open_output_file();

    let svd_path = util::read_relpath_from_env(ENV_SVD_IN)?;
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
