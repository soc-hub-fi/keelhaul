//! Exposes functionality supported by this crate

// Export API types
pub use crate::codegen::{CodegenConfig, FailureImplKind, MemTestStrategy};
pub use crate::filtering::{Filter, Filters, ListFilter, RegexFilter};
pub use crate::model::PtrSize;
pub use error::{ApiError, ParseTestKindError};
pub use svd::ValidateLevel;

mod error;

use std::{ops, path, str};

use crate::{analysis, codegen, model};
use error::NotImplementedError;
use itertools::Itertools;
use log::info;
use strum::EnumIter;

/// A source file for memory map metadata
///
/// Only SVD files are currently supported, with support for IEEE-1685-2014 (IP-XACT 2014) and
/// IEEE-1685-2022 (IP-XACT 2022) planned.
#[derive(Clone, Debug)]
pub struct ModelSource {
    path: path::PathBuf,
    format: SourceFormat,
}

impl ModelSource {
    pub fn new(path: path::PathBuf, format: SourceFormat) -> Self {
        Self { path, format }
    }

    pub fn path(&self) -> &path::Path {
        &self.path
    }
}

#[derive(Clone, Debug)]
pub enum SourceFormat {
    /// CMSIS-SVD (at least v1.3 and below)
    Svd(ValidateLevel),
    /// IP-XACT (2014, 2022)
    Ieee1685,
}

/// Pointer width used by the target architecture
#[derive(Clone, Copy, Debug)]
pub enum ArchWidth {
    /// The target uses a 4-byte -- or 32-bit -- address space
    U32,
    /// The target uses an 8-byte -- or 64-bit -- address space
    U64,
}

impl From<ArchWidth> for model::PtrSize {
    fn from(value: ArchWidth) -> Self {
        match value {
            ArchWidth::U32 => model::PtrSize::U32,
            ArchWidth::U64 => model::PtrSize::U64,
        }
    }
}

/// Type of metadata test generatable by keelhaul
#[derive(Clone, Debug, Hash, PartialEq, Eq, EnumIter)]
pub enum TestKind {
    /// The register value will be read, though nothing will be done with the output
    Read,
    /// The register value will be read and compared compared against the known reset value
    ///
    /// Reset tests are only generated for registers that have a known reset value.
    ReadIsResetVal,
}

impl str::FromStr for TestKind {
    type Err = ParseTestKindError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "read" => Ok(Self::Read),
            "reset" | "read_is_reset_val" => Ok(Self::ReadIsResetVal),
            s => Err(ParseTestKindError(s.to_owned())),
        }
    }
}

/// Run the parser on the inputs without doing anything
///
/// Good for checking whether the input files can be parsed by Keelhaul.
///
/// # Arguments
///
/// * `use_legacy` - Use the original, custom SVD parser instead of the new `svd-parser` based
///   parser
pub fn dry_run(sources: &[ModelSource], arch: ArchWidth, use_legacy: bool) -> Result<(), ApiError> {
    parse_registers(sources, arch, &Filters::all(), false, use_legacy)?;

    Ok(())
}

/// Run the parser on a set of `sources` and return the collection of registers contained within
///
/// # Type arguments
///
/// * `P` - architecture width, i.e., a type that can represent any address on the platform
///
/// # Arguments
///
/// * `use_zero_as_default_reset` - Assume zero as the default reset value if not provided by the
///   source file. Provided for convenience, as `0` is a very common reset value.
/// * `use_legacy` - Use the original, custom SVD parser instead of the new `svd-parser` based
///   parser
fn parse_registers(
    sources: &[ModelSource],
    arch: ArchWidth,
    filters: &Filters,
    use_zero_as_default_reset: bool,
    use_legacy: bool,
) -> Result<model::Registers, ApiError> {
    for src in sources {
        match src.format {
            SourceFormat::Ieee1685 => {
                return Err(NotImplementedError::UnsupportedSourceFormat(
                    src.path.clone(),
                    src.format.clone(),
                )
                .into())
            }
            SourceFormat::Svd(_) => {}
        }
    }

    let mut registers = vec![];

    for src in sources {
        match src.format {
            SourceFormat::Svd(vlevel) => {
                if vlevel != ValidateLevel::Disabled {
                    return Err(
                        NotImplementedError::UnsupportedOption(format!("{:?}", vlevel)).into(),
                    );
                }
                let default_reset_value = use_zero_as_default_reset.then_some(0);

                let regs = if use_legacy {
                    // Safety: max 3 levels of hierarchy (periph + cluster + reg)
                    unsafe {
                        crate::frontend::svd_legacy::parse_svd_into_registers(
                            src.path(),
                            arch.into(),
                            filters,
                            default_reset_value,
                        )
                    }
                    .map_err(Box::new)?
                } else {
                    crate::frontend::svd_parser::parse_svd_into_registers(
                        src.path(),
                        arch.into(),
                        filters,
                        vlevel,
                    )
                    .map_err(Box::new)?
                };
                registers.push(regs)
            }
            SourceFormat::Ieee1685 => todo!(),
        }
    }

    Ok(registers.into_iter().next().unwrap())
}

/// # Arguments
///
/// * `use_legacy` - Use the original, custom SVD parser instead of the new `svd-parser` based
///   parser
fn parse_registers_for_analysis(
    sources: &[ModelSource],
    filters: &Filters,
    arch: ArchWidth,
    use_legacy: bool,
) -> Result<Vec<Box<dyn analysis::AnalyzeRegister>>, ApiError> {
    Ok(parse_registers(sources, arch, filters, false, use_legacy)?
        .clone()
        .into_iter()
        .map(|reg| Box::new(reg) as Box<dyn analysis::AnalyzeRegister>)
        .collect())
}

#[cfg(feature = "rustfmt")]
fn apply_fmt(input: String) -> String {
    info!("Applying rustfmt");
    let mut buf = Vec::new();

    // FIXME: allow supplying config from API
    let formatted = rustfmt::format_input(
        rustfmt::Input::Text(input.clone()),
        &rustfmt::config::Config::default(),
        Some(&mut buf),
    );

    match formatted {
        Ok((_, formatted, _)) => match formatted.into_iter().next() {
            Some((_name, output)) => output.to_string(),
            None => {
                log::error!("rustfmt output was none, returning unformatted output.");
                input
            }
        },
        Err(e) => {
            log::error!("rustfmt failed, returning unformatted output. Error: {e:?}");
            input
        }
    }
}

/// # Arguments
///
/// * `use_legacy` - Use the original, custom SVD parser instead of the new `svd-parser` based
///   parser
pub fn generate_tests(
    sources: &[ModelSource],
    arch_ptr_size: ArchWidth,
    test_cfg: &CodegenConfig,
    filters: &Filters,
    use_zero_as_default_reset: bool,
    use_legacy: bool,
) -> Result<String, ApiError> {
    let registers = parse_registers(
        sources,
        arch_ptr_size,
        filters,
        use_zero_as_default_reset,
        use_legacy,
    )?;
    let test_cases = codegen::RegTestCases::from_registers(&registers, test_cfg);
    // FIXME: it would be good to have this message prior to generation
    info!("Wrote {} test cases.", test_cases.test_case_count);

    Ok(test_cases.to_tokens().to_string())
}

/// # Arguments
///
/// * `use_legacy` - Use the original, custom SVD parser instead of the new `svd-parser` based
///   parser
#[cfg(feature = "rustfmt")]
pub fn generate_tests_with_format(
    sources: &[ModelSource],
    arch_ptr_size: ArchWidth,
    test_cfg: &codegen::CodegenConfig,
    filters: &Filters,
    use_zero_as_default_reset: bool,
    use_legacy: bool,
) -> Result<String, ApiError> {
    let s = generate_tests(
        sources,
        arch_ptr_size,
        test_cfg,
        filters,
        use_zero_as_default_reset,
        use_legacy,
    )?;
    Ok(apply_fmt(s))
}

/// # Arguments
///
/// * `use_legacy` - Use the original, custom SVD parser instead of the new `svd-parser` based
///   parser
pub fn count_registers_svd(
    sources: &[ModelSource],
    arch: ArchWidth,
    filters: &Filters,
    use_legacy: bool,
) -> Result<usize, ApiError> {
    let registers = parse_registers_for_analysis(sources, filters, arch, use_legacy)?;
    Ok(registers.len())
}

/// # Arguments
///
/// * `use_legacy` - Use the original, custom SVD parser instead of the new `svd-parser` based
///   parser
pub fn count_readable_registers_with_reset_value(
    sources: &[ModelSource],
    arch: ArchWidth,
    filters: &Filters,
    use_legacy: bool,
) -> Result<usize, ApiError> {
    let registers = parse_registers_for_analysis(sources, filters, arch, use_legacy)?;
    Ok(registers
        .iter()
        .filter(|reg| reg.is_readable() && reg.has_reset_value())
        .count())
}

/// Returns top level containers (peripherals or subsystems) and the number of registers in each
///
/// `Vec<(container, register count)>`
///
/// # Arguments
///
/// * `use_legacy` - Use the original, custom SVD parser instead of the new `svd-parser` based
///   parser
pub fn list_top(
    sources: &[ModelSource],
    arch: ArchWidth,
    use_legacy: bool,
) -> Result<Vec<(String, usize)>, ApiError> {
    let registers = parse_registers_for_analysis(sources, &Filters::all(), arch, use_legacy)?;

    let tops = registers
        .iter()
        .map(|reg| reg.top_container_name())
        .unique()
        .collect_vec();
    let tops_and_counts = tops
        .into_iter()
        .map(|top| {
            let reg_count = registers
                .iter()
                .filter(|reg| reg.top_container_name() == top)
                .count();
            (top, reg_count)
        })
        .collect_vec();

    Ok(tops_and_counts)
}

pub fn generate_memtests(
    test_ranges: &[ops::Range<u64>],
    strategy: &MemTestStrategy,
    on_fail: &FailureImplKind,
) -> String {
    codegen::gen_memtest_module(test_ranges, 8, strategy, on_fail).to_string()
}

/// # Arguments
///
/// * `format_output` - Format the output using `rustfmt`
#[cfg(feature = "rustfmt")]
pub fn generate_memtests_with_format(
    test_ranges: &[ops::Range<u64>],
    strategy: &MemTestStrategy,
    on_fail: &FailureImplKind,
) -> String {
    let s = generate_memtests(test_ranges, strategy, on_fail);
    apply_fmt(s)
}
