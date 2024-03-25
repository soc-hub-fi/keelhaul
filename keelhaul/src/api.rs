//! Exposes functionality supported by this crate
mod error;

use std::{path, str};

use crate::{error::SvdParseError, generate, model, Filters, TestConfig};
use error::NotImplementedError;
use log::info;

pub use crate::model::{ArchPtr, PtrSize, RefSchemaSvdV1_2, RefSchemaSvdV1_3};
pub use error::ApiError;
pub use svd_parser::ValidateLevel;

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
    /// CMSIS-SVD v1.2 or lower
    SvdV1_2,
    /// CMSIS-SVD v1.3 or lower
    SvdV1_3,
    /// IP-XACT 2014
    Ieee1685_2014,
    /// IP-XACT 2022
    Ieee1685_2022,
}

/// Pointer width used by the target architecture
#[derive(Clone, Copy, Debug)]
pub enum ArchWidth {
    /// The target uses a 4-byte -- or 32-bit -- address space
    U32,
    /// The target uses an 8-byte -- or 64-bit -- address space
    U64,
}

/// Run the parser on the inputs without doing anything
///
/// Good for checking whether the input files can be parsed by Keelhaul.
pub fn dry_run(sources: &[ModelSource], arch: ArchWidth) -> Result<(), ApiError> {
    match arch {
        ArchWidth::U32 => {
            parse_registers::<u32, model::RefSchemaSvdV1_2>(sources, &Filters::all())?;
        }
        ArchWidth::U64 => {
            parse_registers::<u64, model::RefSchemaSvdV1_2>(sources, &Filters::all())?;
        }
    }
    Ok(())
}

type Registers<P> = model::Registers<P, RefSchemaSvdV1_2>;

/// Run the parser on a set of `sources` and return the collection of registers contained within
///
/// # Type arguments
///
/// * `P` - architecture width, i.e., a type that can represent any address on the platform
fn parse_registers<P, S>(
    sources: &[ModelSource],
    filters: &Filters,
) -> Result<Registers<P>, ApiError>
where
    P: model::ArchPtr,
    SvdParseError: From<<P as num::Num>::FromStrRadixErr>
        + From<<P as str::FromStr>::Err>
        + From<<P as TryFrom<u64>>::Error>,
    <P as TryFrom<u64>>::Error: std::fmt::Debug,
{
    for src in sources {
        match src.format {
            SourceFormat::Ieee1685_2014 | SourceFormat::Ieee1685_2022 => {
                return Err(NotImplementedError::UnsupportedSourceFormat(
                    src.path.clone(),
                    src.format.clone(),
                )
                .into())
            }
            _ => {}
        }
    }

    let mut registers = vec![];

    for src in sources {
        match src.format {
            SourceFormat::SvdV1_2 => registers.push(
                crate::frontend::svd_v1_2::parse_svd_into_registers::<P>(src.path(), &filters)?,
            ),
            SourceFormat::SvdV1_3 => {
                /*
                // TODO: needs to be wrapped in an enum
                registers.push(Registers::SvdV1_3(crate::frontend::svd_v1_3::parse_svd_into_registers::<P>(
                    src.path(),
                    &filters,
                    validate_level,
                )?)
                */
                todo!()
            }
            SourceFormat::Ieee1685_2014 => todo!(),
            SourceFormat::Ieee1685_2022 => todo!(),
        }
    }

    Ok(registers.into_iter().nth(0).unwrap())
}

pub fn generate_tests(
    sources: &[ModelSource],
    arch_ptr_size: PtrSize,
    test_cfg: &TestConfig,
    filters: &Filters,
) -> Result<String, ApiError> {
    let test_cases: generate::TestCases = match arch_ptr_size {
        PtrSize::U8 => {
            let registers = parse_registers::<u8, RefSchemaSvdV1_2>(sources, filters)?;
            generate::TestCases::from_registers(&registers, &test_cfg)
        }
        PtrSize::U16 => {
            let registers = parse_registers::<u16, RefSchemaSvdV1_2>(sources, filters)?;
            generate::TestCases::from_registers(&registers, &test_cfg)
        }
        PtrSize::U32 => {
            let registers = parse_registers::<u32, RefSchemaSvdV1_2>(sources, filters)?;
            generate::TestCases::from_registers(&registers, &test_cfg)
        }
        PtrSize::U64 => {
            let registers = parse_registers::<u64, RefSchemaSvdV1_2>(sources, filters)?;
            generate::TestCases::from_registers(&registers, &test_cfg)
        }
    }
    .unwrap();
    info!("Wrote {} test cases.", test_cases.test_case_count);
    Ok(test_cases.to_module_string())
}
