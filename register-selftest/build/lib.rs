//! Memory-mapped I/O peripheral register test case generator.

mod logger;
mod util;

use std::{
    collections::HashSet,
    env,
    io::{self, Write},
    path::{Path, PathBuf},
    str::FromStr,
};

use anyhow::Context;
use fs_err::{self as fs, File};
use keelhaul::{
    CodegenConfig, Filters, ListFilter, ModelSource, ParseTestKindError, RegexFilter, TestKind,
};
use log::LevelFilter;
use regex::Regex;

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

fn test_types_from_env() -> Result<Option<HashSet<TestKind>>, ParseTestKindError> {
    let test_kinds = util::read_vec_from_env(ENV_TEST_KINDS, ',');
    if let Ok(test_kinds) = test_kinds {
        Ok(Some(
            test_kinds
                .into_iter()
                .map(|test_kind| TestKind::from_str(&test_kind))
                .collect::<Result<HashSet<_>, ParseTestKindError>>()?,
        ))
    } else {
        Ok(None)
    }
}

fn main() -> anyhow::Result<()> {
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

    let arch_ptr_size_bytes = match util::read_u32_from_env(ENV_ARCH)
        .with_context(|| format!("could not detect {ENV_ARCH}"))?
    {
        4 => keelhaul::ArchWidth::U32,
        8 => keelhaul::ArchWidth::U64,
        _ => panic!("unsupported arch size"),
    };
    let mut test_cfg = CodegenConfig::default();
    if let Some(test_kind_set) =
        test_types_from_env().with_context(|| format!("Could not detect {ENV_TEST_KINDS}"))?
    {
        test_cfg = test_cfg.tests_to_generate(test_kind_set).unwrap();
    }
    let mut file_output = open_output_file();

    let periph_filter = {
        let include_peripherals = util::read_vec_from_env(ENV_INCLUDE_PERIPHS, ',').ok();
        let exclude_peripherals = util::read_vec_from_env(ENV_EXCLUDE_PERIPHS, ',').ok();
        Box::new(ListFilter::new(
            include_peripherals,
            exclude_peripherals.unwrap_or_default(),
        ))
    };
    let syms_filter: Option<Box<dyn keelhaul::Filter>> = env::var(ENV_INCLUDE_SYMS_REGEX)
        .ok()
        .map(|s| Regex::new(&s))
        .transpose()?
        .map(RegexFilter::new)
        .map(|f| -> Box<dyn keelhaul::Filter> { Box::new(f) });

    let svd_path = util::read_relpath_from_env(ENV_SVD_IN)
        .with_context(|| format!("Could not detect {ENV_SVD_IN}"))?;
    let test_cases = keelhaul::generate_tests(
        &[ModelSource::new(
            svd_path,
            keelhaul::SourceFormat::Svd(keelhaul::ValidateLevel::Disabled),
        )],
        arch_ptr_size_bytes,
        &test_cfg,
        &Filters::from_filters(None, Some(periph_filter), syms_filter),
        true,
        true,
    )?;
    file_output.write_all(test_cases.as_bytes())?;
    let path = get_path_to_output();
    rustfmt_file(&path).unwrap_or_else(|error: io::Error| {
        panic!("Failed to format file {}. {}", path.display(), error)
    });
    Ok(())
}
