//! Exposes functionality supported by this crate
mod error;

use std::{path, str};

use crate::{error::SvdParseError, model, Filters};
use error::NotImplementedError;

pub use error::ApiError;

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
    /// CMSIS-SVD
    Svd,
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
            parse_registers::<u32>(sources, Filters::all())?;
        }
        ArchWidth::U64 => {
            parse_registers::<u64>(sources, Filters::all())?;
        }
    }
    Ok(())
}

/// Run the parser on a set of `sources` and return the collection of registers contained within
///
/// # Type arguments
///
/// * `P` - the architecture width as represented by u32 or u64
pub fn parse_registers<P>(
    sources: &[ModelSource],
    filters: Filters,
) -> Result<Vec<model::Registers<P>>, ApiError>
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

    let registers = sources
        .iter()
        .map(|src| crate::parse_svd::parse_svd_into_registers::<P>(src.path(), &filters))
        .collect::<Result<Vec<_>, crate::error::Error>>()?;
    Ok(registers)
}
