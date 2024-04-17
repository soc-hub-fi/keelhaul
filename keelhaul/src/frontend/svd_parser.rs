use std::{fmt, iter, path};

use crate::{
    error::Error,
    model::{self, AddrRepr, RegPath, RegPathSegment, Register, Registers, UniquePath},
    util, Filters, PtrSize,
};
use itertools::Itertools;
use log::info;
use svd::Name;

// TODO: error handling: check for unwrap, expect

/// Property chain element
///
/// Helps with traversing the hierarchy for specific attributes.
trait PropChainElem: svd::Name + svd::Description + fmt::Debug {
    /// Address offset, i.e., base address or offset
    fn offset(&self) -> Option<u64>;
    /// Size of the register
    fn size(&self) -> Option<u32>;
    fn access(&self) -> Option<svd::Access>;
    fn reset_value(&self) -> Option<u64>;
    fn reset_mask(&self) -> Option<u64>;
}

impl PropChainElem for svd::Device {
    fn offset(&self) -> Option<u64> {
        None
    }

    fn size(&self) -> Option<u32> {
        self.default_register_properties.size
    }

    fn access(&self) -> Option<svd::Access> {
        self.default_register_properties.access
    }

    fn reset_value(&self) -> Option<u64> {
        self.default_register_properties.reset_value
    }

    fn reset_mask(&self) -> Option<u64> {
        self.default_register_properties.reset_mask
    }
}

impl PropChainElem for svd::Peripheral {
    fn offset(&self) -> Option<u64> {
        Some(self.base_address)
    }

    fn size(&self) -> Option<u32> {
        self.default_register_properties.size
    }

    fn access(&self) -> Option<svd::Access> {
        self.default_register_properties.access
    }

    fn reset_value(&self) -> Option<u64> {
        self.default_register_properties.reset_value
    }

    fn reset_mask(&self) -> Option<u64> {
        self.default_register_properties.reset_mask
    }
}

impl PropChainElem for svd::Cluster {
    fn offset(&self) -> Option<u64> {
        Some(self.address_offset as u64)
    }

    fn size(&self) -> Option<u32> {
        self.default_register_properties.size
    }

    fn access(&self) -> Option<svd::Access> {
        self.default_register_properties.access
    }

    fn reset_value(&self) -> Option<u64> {
        self.default_register_properties.reset_value
    }

    fn reset_mask(&self) -> Option<u64> {
        self.default_register_properties.reset_mask
    }
}

impl PropChainElem for svd::Register {
    fn offset(&self) -> Option<u64> {
        Some(self.address_offset as u64)
    }

    fn size(&self) -> Option<u32> {
        self.properties.size
    }

    fn access(&self) -> Option<svd::Access> {
        self.properties.access
    }

    fn reset_value(&self) -> Option<u64> {
        self.properties.reset_value
    }

    fn reset_mask(&self) -> Option<u64> {
        self.properties.reset_mask
    }
}

fn to_reg(
    arch_size: u32,
    device: &svd::Device,
    periph: &svd::Peripheral,
    cluster_chain: &[svd::Cluster],
    reg: &svd::Register,
) -> Register {
    let parent_chain = iter::once(device as &dyn PropChainElem)
        .chain(iter::once(periph as &dyn PropChainElem))
        .chain(cluster_chain.iter().map(|cl| cl as &dyn PropChainElem))
        .chain(iter::once(reg as &dyn PropChainElem))
        .collect_vec();
    let path = RegPath::new(
        parent_chain
            .iter()
            // Skip the device for path resolution
            .skip(1)
            .map(|p| RegPathSegment {
                name: p.name().replace("[%s]", "placeholder"),
            })
            .collect(),
    );
    let addr = AddrRepr::from_vec(
        parent_chain
            .iter()
            // Skip the device for address formation
            .skip(1)
            .filter_map(|p| p.offset())
            .collect(),
        arch_size,
    )
    .expect("could not make address");
    let size = parent_chain
        .iter()
        .rev()
        .find_map(|p| p.size())
        .unwrap_or_else(|| {
            panic!(
                "could not determine size for register at path {:?}, chain: {:#?}",
                path.join("-"),
                parent_chain
            )
        });
    let access = parent_chain
        .iter()
        .rev()
        .find_map(|p| p.access())
        .unwrap_or(svd::Access::ReadWrite);
    let reset_value = parent_chain.iter().rev().find_map(|p| p.reset_value());
    let reset_mask = parent_chain.iter().rev().find_map(|p| p.reset_mask());
    let reset_value = match (reset_value, reset_mask) {
        // CMSIS-SVD requires both value and mask to be present
        (Some(val), Some(mask)) => Some(model::ValueOnReset::new(val, Some(mask), size)),
        _ => None,
    };
    Register::new(path, addr, size, access, reset_value)
}

// Recursive
fn select_regs_from_cluster(
    device: &svd::Device,
    periph: &svd::Peripheral,
    cluster_chain: &[svd::Cluster],
    size: u32,
) -> Vec<Register> {
    let last = cluster_chain
        .last()
        .expect("internal error: chain cannot be empty");
    last
        // Direct child registers
        .registers()
        .map(|reg| to_reg(size, device, periph, cluster_chain, reg))
        .chain(
            // Registers via clusters
            last.clusters().flat_map(|cluster| {
                let mut cluster_chain = cluster_chain.to_vec();
                cluster_chain.push(cluster.clone());
                select_regs_from_cluster(device, periph, &cluster_chain, size)
            }),
        )
        .collect::<Vec<_>>()
}

/// # Arguments
///
/// * `size` - Address representation
fn select_regs_from_periphs(
    device: &svd::Device,
    periphs: &[svd::Peripheral],
    size: u32,
) -> Vec<Register> {
    info!(
        "Scanning {} peripherals: {}",
        periphs.len(),
        periphs.iter().map(|p| p.name()).join(", ")
    );

    periphs
        .iter()
        .flat_map(|periph| {
            periph
                // Direct child registers
                .registers()
                .map(|reg| to_reg(size, device, periph, &[], reg))
                .chain(
                    // Registers via clusters
                    periph.clusters().flat_map(|cluster| {
                        select_regs_from_cluster(device, periph, &vec![cluster.clone()], size)
                    }),
                )
        })
        .collect()
}

/// Parse the file at `svd_path` into a list of registers with provided filters & constraints
///
/// # Arguments
///
/// * `svd_path`        - The path to the SVD file
/// * `reg_filter`      - What registers to include or exclude
/// * `periph_filter`   - What peripherals to include or exclude
/// * `syms_filter` -   - What symbols to include or exclude (applying to full register identifier)
pub(crate) fn parse_svd_into_registers(
    svd_source: &path::Path,
    arch: PtrSize,
    filters: &Filters,
    validate_level: crate::ValidateLevel,
) -> Result<Registers, Error> {
    let svd_xml = util::read_file_or_panic(svd_source);

    let parse_config = svd_parser::Config::default();
    parse_config.validate_level(validate_level);
    parse_config.ignore_enums(false);

    // Derive register properties from parents
    parse_config.expand_properties(true);

    // Always expand `arrays` and `deriveFrom`'s as we are only interested in the final output form
    parse_config.expand(true);

    // TODO: error handling
    let device = svd_parser::parse_with_config(&svd_xml, &parse_config).unwrap();

    // Filter out top-level elements
    let mut top_elems = device.peripherals.clone();
    if let Some(f) = filters.top.as_ref() {
        let total = top_elems.len();
        top_elems = top_elems
            .into_iter()
            .filter(|p| f.is_allowed(p.name()))
            .collect_vec();
        let selected = top_elems.len();
        info!(
            "Top filter selected {selected}/{total} of top elements: {}",
            top_elems.iter().map(|p| p.name()).join(", "),
        );
    }

    // TODO: error handling
    let mut registers = select_regs_from_periphs(&device, &top_elems, arch.count_bits());

    if let Some(f) = filters.path.as_ref() {
        let remaining = registers.len();
        registers = registers
            .into_iter()
            .filter(|reg| {
                f.is_allowed(
                    &reg.path()
                        // Dash '-' is used as the path combiner
                        .join("-"),
                )
            })
            .collect_vec();
        let selected = registers.len();
        info!(
            "Path filter selected {selected}/{remaining} of remaining registers: {}",
            top_elems.iter().map(|p| p.name()).join(", "),
        );
    }

    // If zero registers were chosen for generation, this run is useless.
    // Therefore we treat it as an error.
    // TODO: allow ignoring this error for special cases with a suitable flag on Config-struct
    if registers.is_empty() {
        return Err(Error::ZeroEntries);
    }

    info!("Discovered {} registers", registers.len());
    Ok(registers.into())
}
