//! SVD-file parser for register test generator.

use crate::{
    read_excludes_from_env, read_file_or_panic, read_vec_from_env, Access, AddrOverflowError,
    AddrRepr, Error, IncompatibleTypesError, ItemFilter, NotImplementedError, PositionalError,
    Protection, PtrSize, RegPath, RegValue, Register, RegisterDimElementGroup,
    RegisterPropertiesGroup, Registers, ResetValue, SvdParseError,
};
use itertools::Itertools;
use log::{debug, info, warn};
use regex::Regex;
use roxmltree::{Document, Node};
use std::{
    collections::{hash_map::Entry, HashMap, HashSet},
    env, panic,
    path::PathBuf,
    str::FromStr,
};

/// Find a child node with given tag name.
fn find_text_in_node_by_tag_name<'a>(
    node: &'a Node,
    tag: &str,
) -> Result<(&'a str, Node<'a, 'a>), PositionalError<SvdParseError>> {
    maybe_find_text_in_node_by_tag_name(node, tag).ok_or(
        SvdParseError::ExpectedTagInElement {
            elem_name: node.tag_name().name().to_owned(),
            tag: tag.to_owned(),
        }
        .with_byte_pos_range(node.range(), node.document()),
    )
}

/// Try to find a child node with given name.
fn maybe_find_text_in_node_by_tag_name<'a>(
    node: &'a Node,
    tag: &str,
) -> Option<(&'a str, Node<'a, 'a>)> {
    node.children()
        .find(|n| n.has_tag_name(tag))
        .map(|n| (n.text().expect("Node does not have text."), n))
}

fn binary_size_mult_from_char(c: char) -> Result<u64, SvdParseError> {
    match c {
        'k' | 'K' => Ok(1024),
        'm' | 'M' => Ok(1024 * 1024),
        'g' | 'G' => Ok(1024 * 1024 * 1024),
        't' | 'T' => Ok(1024 * 1024 * 1024 * 1024),
        _ => Err(SvdParseError::InvalidSizeMultiplierSuffix(c)),
    }
}

#[test]
fn parse_nonneg_int_u64_works() {
    assert_eq!(parse_nonneg_int_u64("0xFFB00000").unwrap(), 0xFFB00000);
    assert_eq!(parse_nonneg_int_u64("+0xFFB00000").unwrap(), 0xFFB00000);
    // TODO: this test case is invalid. # means binary not hex, and the parser is faulty
    assert_eq!(parse_nonneg_int_u64("#FFB00000").unwrap(), 0xFFB00000);
    assert_eq!(parse_nonneg_int_u64("42").unwrap(), 42);
    assert_eq!(parse_nonneg_int_u64("1 k").unwrap(), 1024);
    assert_eq!(parse_nonneg_int_u64("437260288").unwrap(), 437260288);
}

/// Parses an integer from `text`
///
/// This implementation is format aware and uses regex to ensure correct behavior.
fn parse_nonneg_int_u64(text: &str) -> Result<u64, SvdParseError> {
    // Compile Regexes only once as recommended by the documentation of the Regex crate
    use lazy_static::lazy_static;
    lazy_static! {
        // [0x|0X|\#]{1}          # hexadecimal prefix
        /// Regular expression to capture hexadecimal numbers, as defined in CMSIS-SVD schema
        static ref HEX_NONNEG_INT_RE: Regex = Regex::new(
            r"(?x)              # insignificant whitespace
            \+?                 # zero or one plus sign
            (?:0x|0X|\#)        # hexadecimal prefix
            ([[:xdigit:]]+)     # one or more hexadecimal digits (captured as #1)
            [[:space:]]?        # zero or one of whitespace
            ([kmgtKMGT])?       # zero or one of kilo, mega, giga, tera identifier (captured as #2)
        ").unwrap();

        /// Regular expression to capture decimal numbers, as defined in CMSIS-SVD schema
        static ref DEC_NONNEG_INT_RE: Regex = Regex::new(
            r"(?x)              # insignificant whitespace
            \+?                 # zero or one plus sign
            ([[:digit:]]+)      # one or more decimal digits (captured as #1)
            [[:space:]]?        # zero or one of whitespace
            ([kmgtKMGT])?       # zero or one of kilo, mega, giga, tera identifier (captured as #2)
        ").unwrap();
    }

    // Pick either hexadecimal or decimal format based on which fits
    // TODO: pick binary format on '#'

    let (number_part, size_mult_capture) = if HEX_NONNEG_INT_RE.is_match(text) {
        // Safety: we checked above that at least one match exists in text
        let captures = HEX_NONNEG_INT_RE.captures_iter(text).next().unwrap();

        let digits = &captures[1];
        let number = u64::from_str_radix(digits, 16)?;

        let size_mult = captures.get(2);
        (number, size_mult)
    } else if DEC_NONNEG_INT_RE.is_match(text) {
        // Safety: we checked above that at least one match exists in text
        let captures = DEC_NONNEG_INT_RE.captures_iter(text).next().unwrap();

        let digits = &captures[1];
        let number = digits.parse::<u64>()?;

        let size_mult = captures.get(2);
        (number, size_mult)
    } else {
        return Err(SvdParseError::InvalidNonnegInt(text.to_owned()));
    };

    let size_mult: Option<u64> = size_mult_capture
        // Safety: we know from the regex that there is only one possible size mult char
        .map(|s| s.as_str().chars().next().unwrap())
        .map(binary_size_mult_from_char)
        .transpose()?;

    Ok(match size_mult {
        Some(mult) => number_part * mult,
        None => number_part,
    })
}

#[derive(Clone)]
struct RegPropGroupBuilder {
    /// Register bit-width.
    pub size: Option<PtrSize>,
    /// Register access rights.
    pub access: Option<Access>,
    /// Register access privileges.
    pub protection: Option<Protection>,
    /// Register value after reset.
    /// Actual reset value is calculated using reset value and reset mask.
    pub(crate) reset_value: Option<RegValue>,
    /// Register bits with defined reset value are marked as high.
    pub(crate) reset_mask: Option<RegValue>,
}

fn err_with_pos(e: impl Into<SvdParseError>, node: &Node) -> PositionalError<SvdParseError> {
    e.into().with_byte_pos_range(node.range(), node.document())
}

impl RegPropGroupBuilder {
    fn try_from_periph_node(periph_node: &Node) -> Result<Self, PositionalError<SvdParseError>> {
        let size = match maybe_find_text_in_node_by_tag_name(periph_node, "size") {
            Some((size, size_node)) => {
                let bit_count = size.parse().map_err(|e| err_with_pos(e, &size_node))?;
                Some(PtrSize::from_bit_count(bit_count).ok_or_else(|| {
                    err_with_pos(SvdParseError::BitCountToPtrWidth(bit_count), &size_node)
                })?)
            }
            None => None,
        };
        let access = match maybe_find_text_in_node_by_tag_name(periph_node, "access") {
            Some((access, access_node)) => {
                Some(Access::from_str(access).map_err(|e| err_with_pos(e, &access_node))?)
            }
            None => None,
        };
        let protection = match maybe_find_text_in_node_by_tag_name(periph_node, "protection") {
            Some((prot, prot_node)) => {
                Some(Protection::from_str(prot).map_err(|e| err_with_pos(e, &prot_node))?)
            }
            None => None,
        };
        let reset_value = match maybe_find_text_in_node_by_tag_name(periph_node, "resetValue") {
            Some((reset_val, reset_val_node)) => Some(RegValue::U64(
                parse_nonneg_int_u64(reset_val).map_err(|e| err_with_pos(e, &reset_val_node))?,
            )),
            None => None,
        };
        let reset_mask = match maybe_find_text_in_node_by_tag_name(periph_node, "resetMask") {
            Some((reset_mask, reset_mask_node)) => Some(RegValue::U64(
                parse_nonneg_int_u64(reset_mask).map_err(|e| err_with_pos(e, &reset_mask_node))?,
            )),
            None => None,
        };
        Ok(RegPropGroupBuilder {
            size,
            access,
            protection,
            reset_value,
            reset_mask,
        })
    }

    /// Inherit properties from parent and update with current node's properties if defined.
    ///
    /// # Arguments
    ///
    /// * `node` - can be either cluster or register node
    fn inherit_and_update_from(
        &self,
        node: &Node,
    ) -> Result<RegPropGroupBuilder, PositionalError<SvdParseError>> {
        let mut properties = self.clone();
        if let Some((size, size_node)) = maybe_find_text_in_node_by_tag_name(node, "size") {
            let bit_count = size.parse().map_err(|e| err_with_pos(e, &size_node))?;
            properties.size = Some(PtrSize::from_bit_count(bit_count).ok_or_else(|| {
                err_with_pos(SvdParseError::BitCountToPtrWidth(bit_count), &size_node)
            })?);
        };
        if let Some((access, access_node)) = maybe_find_text_in_node_by_tag_name(node, "access") {
            properties.access =
                Some(Access::from_str(access).map_err(|e| err_with_pos(e, &access_node))?);
        };
        if let Some((prot, prot_node)) = maybe_find_text_in_node_by_tag_name(node, "protection") {
            properties.protection =
                Some(Protection::from_str(prot).map_err(|e| err_with_pos(e, &prot_node))?);
        };
        if let Some((reset_val, reset_val_node)) =
            maybe_find_text_in_node_by_tag_name(node, "resetValue")
        {
            properties.reset_value = Some(RegValue::U64(
                parse_nonneg_int_u64(reset_val).map_err(|e| err_with_pos(e, &reset_val_node))?,
            ));
        };
        if let Some((reset_mask, reset_mask_node)) =
            maybe_find_text_in_node_by_tag_name(node, "resetMask")
        {
            properties.reset_mask = Some(RegValue::U64(
                parse_nonneg_int_u64(reset_mask).map_err(|e| err_with_pos(e, &reset_mask_node))?,
            ));
        };
        Ok(properties)
    }

    pub(crate) fn build(
        self,
        reg_path: &str,
    ) -> Result<RegisterPropertiesGroup, IncompatibleTypesError> {
        let value_size = self.size.unwrap_or_else(|| {
            warn!("register {reg_path} or it's parents have not defined size. Size is assumed to be 'u32'.");
            PtrSize::U32
        });
        let access = self.access.unwrap_or_else(|| {
            warn!("register {reg_path} or it's parents have not defined access. Access is assumed to be 'read-write'.");
            Access::ReadWrite
        });
        let protection = self.protection.unwrap_or_else(|| {
            // This is a very common omission from SVD. We should not warn about it unless required by user
            // TODO: allow changing this to warn! or error! via top level config
            debug!("register {reg_path} or it's parents have not defined protection. Protection is assumed to be 'NonSecureOrSecure'.");
            Protection::NonSecureOrSecure
        });
        let reset_value = self.reset_value.unwrap_or_else(|| {
            warn!("register {reg_path} or it's parents have not defined reset value. Reset value is assumed to be '0'.");
            match value_size {
                PtrSize::U8 => RegValue::U8(0),
                PtrSize::U16 => RegValue::U16(0),
                PtrSize::U32 => RegValue::U32(0),
                PtrSize::U64 => RegValue::U64(0),
            }
        });
        let reset_mask = self.reset_mask.unwrap_or_else(|| {
            warn!("register {reg_path} or it's parents have not defined reset mask. Reset mask is assumed to be '{}::MAX'.", value_size);
            match value_size {
                PtrSize::U8 => RegValue::U8(u8::MAX),
                PtrSize::U16 => RegValue::U16(u16::MAX),
                PtrSize::U32 => RegValue::U32(u32::MAX),
                PtrSize::U64 => RegValue::U64(u64::MAX),
            }
        });
        let reset_value = ResetValue::with_mask(reset_value, reset_mask)?;

        Ok(RegisterPropertiesGroup::new(
            value_size,
            access,
            protection,
            reset_value,
        ))
    }
}

// The presence of this pattern in the register name likely indicates that this
// is an array register
//
// TODO: should use a more robust way of detecting arrays, i.e., checking the
// fields for the reg in question
const SVD_ARRAY_REPETITION_PATTERN: &str = "%s";

enum RegisterParentKind {
    Periph,
    Cluster {
        cluster_name: String,
        cluster_offset: u64,
    },
}

struct RegisterParent {
    kind: RegisterParentKind,
    periph_name: String,
    periph_base: u64,
    properties: RegPropGroupBuilder,
}

impl RegisterParent {
    fn from_periph_node(periph_node: &Node) -> Result<Self, PositionalError<SvdParseError>> {
        let (base_addr_str, base_addr_node) =
            find_text_in_node_by_tag_name(periph_node, "baseAddress")?;
        let base_addr =
            parse_nonneg_int_u64(base_addr_str).map_err(|e| err_with_pos(e, &base_addr_node))?;
        let (periph_name, _) = find_text_in_node_by_tag_name(periph_node, "name")?;

        Ok(Self {
            periph_name: periph_name.to_string(),
            periph_base: base_addr,
            properties: RegPropGroupBuilder::try_from_periph_node(periph_node)?,
            kind: RegisterParentKind::Periph,
        })
    }

    fn inherit_and_update_from_cluster(
        &self,
        cluster_node: &Node,
    ) -> Result<Self, PositionalError<SvdParseError>> {
        let (cluster_name, _) = find_text_in_node_by_tag_name(cluster_node, "name")?;
        let (cluster_offset_str, cluster_offset_node) =
            find_text_in_node_by_tag_name(cluster_node, "addressOffset")?;
        let cluster_offset = parse_nonneg_int_u64(cluster_offset_str)
            .map_err(|e| err_with_pos(e, &cluster_offset_node))?;

        Ok(Self {
            periph_name: self.periph_name.clone(),
            periph_base: self.periph_base,
            properties: self.properties.inherit_and_update_from(cluster_node)?,
            kind: RegisterParentKind::Cluster {
                cluster_name: cluster_name.to_string(),
                cluster_offset,
            },
        })
    }
}

impl TryFrom<&Node<'_, '_>> for RegisterDimElementGroup {
    type Error = PositionalError<SvdParseError>;

    fn try_from(value: &Node) -> Result<Self, Self::Error> {
        let (dim, dim_node) = find_text_in_node_by_tag_name(value, "dim")?;
        let dim = parse_nonneg_int_u64(dim).map_err(|e| err_with_pos(e, &dim_node))?;
        let (dim_inc, dim_inc_node) = find_text_in_node_by_tag_name(value, "dimIncrement")?;
        let dim_increment =
            parse_nonneg_int_u64(dim_inc).map_err(|e| err_with_pos(e, &dim_inc_node))?;
        Ok(Self { dim, dim_increment })
    }
}

fn process_register(
    parent: &RegisterParent,
    register_node: Node,
    reg_filter: &ItemFilter<String>,
    syms_regex: &ItemFilter<String>,
) -> Result<Option<Register<u32>>, PositionalError<SvdParseError>> {
    let (name, _) = find_text_in_node_by_tag_name(&register_node, "name")?;
    let name = name.to_owned();
    let (addr_offset_str, addr_offset_node) =
        find_text_in_node_by_tag_name(&register_node, "addressOffset")?;
    let addr_offset =
        parse_nonneg_int_u64(addr_offset_str).map_err(|e| err_with_pos(e, &addr_offset_node))?;

    //let reg_name = remove_illegal_characters(reg_name);
    let path = RegPath::from_components(
        parent.periph_name.clone(),
        match &parent.kind {
            RegisterParentKind::Periph => None,
            RegisterParentKind::Cluster { cluster_name, .. } => Some(cluster_name.clone()),
        },
        name.clone(),
    );
    let reg_path = path.join("-");

    if syms_regex.is_blocked(&reg_path) {
        info!("Register {reg_path} was not included due to regex set in SYMS_REGEX");
        return Ok(None);
    }

    // FIXME: we match against only the register's name, not the path. This is not a
    // great way to exclude registers. We should match against the entire path.
    if reg_filter.is_blocked(&name) {
        info!("register {name} is was not included due to values set in PATH_EXCLUDES");
        return Ok(None);
    }

    if name.contains(SVD_ARRAY_REPETITION_PATTERN) {
        warn!("{}, skipping", NotImplementedError::SvdArray(reg_path));
        return Ok(None);
    }

    let properties = parent.properties.inherit_and_update_from(&register_node)?;
    let properties = properties
        .build(&reg_path)
        .map_err(|e| err_with_pos(e, &register_node))?;

    let addr = AddrRepr::<u64>::new(
        parent.periph_base,
        match parent.kind {
            RegisterParentKind::Periph => None,
            RegisterParentKind::Cluster { cluster_offset, .. } => Some(cluster_offset),
        },
        addr_offset,
    );
    let addr = AddrRepr::<u32>::try_from(addr.clone())
        .map_err(|_| AddrOverflowError::new(path.join("-"), addr.clone()))
        .map_err(|e| err_with_pos(e, &register_node))?;
    let dimensions = match RegisterDimElementGroup::try_from(&register_node) {
        Ok(dimensions) => Some(dimensions),
        Err(_) => None,
    };

    let register = Register {
        path,
        addr,
        properties,
        dimensions,
    };
    Ok(Some(register))
}

fn process_cluster(
    parent: &RegisterParent,
    cluster_node: Node,
    reg_filter: &ItemFilter<String>,
    syms_regex: &ItemFilter<String>,
) -> Result<Option<Vec<Register<u32>>>, PositionalError<SvdParseError>> {
    let current_parent = parent.inherit_and_update_from_cluster(&cluster_node)?;

    let mut registers = Vec::new();
    for register_node in cluster_node
        .children()
        .filter(|n| n.has_tag_name("register"))
    {
        if let Some(register) =
            process_register(&current_parent, register_node, reg_filter, syms_regex)?
        {
            registers.push(register);
        }
    }
    Ok(Some(registers))
}

fn process_peripheral(
    periph_node: Node,
    periph_filter: &ItemFilter<String>,
    reg_filter: &ItemFilter<String>,
    syms_regex: &ItemFilter<String>,
) -> Result<Option<Vec<Register<u32>>>, PositionalError<SvdParseError>> {
    let periph = RegisterParent::from_periph_node(&periph_node)?;
    let periph_name = &periph.periph_name;

    if periph_filter.is_blocked(&periph_name.to_lowercase()) {
        info!("Peripheral {periph_name} was not included due to values set in INCLUDE_PERIPHERALS and/or EXCLUDE_PERIPHERALS");
        return Ok(None);
    }

    let registers_nodes = periph_node
        .children()
        .filter(|n| n.has_tag_name("registers"))
        .collect_vec();
    assert!(
        registers_nodes.len() == 1,
        "SVD file peripheral node must contains one registers node."
    );
    let registers_node = registers_nodes.first().unwrap();

    let mut registers = Vec::new();
    for cluster_node in registers_node
        .children()
        .filter(|n| n.has_tag_name("cluster"))
    {
        if let Some(cluster_registers) =
            process_cluster(&periph, cluster_node, reg_filter, syms_regex)?
        {
            registers.extend(cluster_registers);
        }
    }
    for register_node in registers_node
        .children()
        .filter(|n| n.has_tag_name("register"))
    {
        if let Some(register) = process_register(&periph, register_node, reg_filter, syms_regex)? {
            registers.push(register);
        }
    }
    Ok(Some(registers))
}

/// Find registers from SVD XML-document.
fn find_registers(
    parsed: &Document,
    reg_filter: &ItemFilter<String>,
    periph_filter: &ItemFilter<String>,
    syms_regex: &ItemFilter<String>,
) -> Result<Registers<u32>, PositionalError<SvdParseError>> {
    let device_nodes = parsed
        .root()
        .children()
        .filter(|n| n.has_tag_name("device"))
        .collect_vec();
    assert!(
        device_nodes.len() == 1,
        "SVD file must contain one device node."
    );
    let device_node = device_nodes.first().unwrap();

    let peripherals_nodes = device_node
        .children()
        .filter(|n| n.has_tag_name("peripherals"))
        .collect_vec();
    assert!(
        peripherals_nodes.len() == 1,
        "SVD file must contains one peripherals node."
    );
    let peripherals_node = peripherals_nodes.first().unwrap();

    let mut registers = Vec::new();
    for peripheral_node in peripherals_node
        .children()
        .filter(|n| n.has_tag_name("peripheral"))
    {
        if let Some(peripheral_registers) =
            process_peripheral(peripheral_node, periph_filter, reg_filter, syms_regex)?
        {
            registers.extend(peripheral_registers);
        }
    }

    let mut peripherals = HashSet::new();
    let mut addresses = HashMap::new();
    for register in &registers {
        peripherals.insert(register.path.periph.clone());
        if let Entry::Vacant(entry) = addresses.entry(register.full_addr().unwrap()) {
            entry.insert(register.path.join("-"));
        } else {
            let address_holder = addresses
                .get(&register.full_addr().unwrap())
                .expect("failed to find register name by key");
            warn!("register {}'s full address is already taken by register {address_holder}. This register is ignored.", register.path.join("-"));
        }
    }

    info!("Found {} peripherals:", peripherals.len());
    for peripheral in peripherals {
        info!("    {peripheral}");
    }
    Ok(registers.into())
}

/// Parse SVD-file.
pub fn parse() -> Result<Registers<u32>, Error> {
    let include_peripherals = read_vec_from_env("INCLUDE_PERIPHERALS", ',');
    let exclude_peripherals = read_vec_from_env("EXCLUDE_PERIPHERALS", ',');
    let periph_filter =
        ItemFilter::list(include_peripherals, exclude_peripherals.unwrap_or(vec![]));
    let include_syms_regex = env::var("INCLUDE_SYMS_REGEX")
        .ok()
        .map(|s| Regex::new(&s))
        .transpose()?;
    let exclude_syms_regex = env::var("EXCLUDE_SYMS_REGEX")
        .ok()
        .map(|s| Regex::new(&s))
        .transpose()?;
    let syms_filter = ItemFilter::regex(include_syms_regex, exclude_syms_regex);

    let reg_filter = ItemFilter::list(None, read_excludes_from_env().unwrap_or(vec![]));

    let svd_fname = env::var("SVD_PATH").unwrap_or_else(|_| {
        env::var("PATH_SVD")
            .map(|p| {
                warn!("PATH_SVD is under threat of deprecation, use SVD_PATH instead");
                p
            })
            .unwrap_or_else(|err| panic!("PATH_SVD or SVD_PATH must be set: {err}"))
    });
    let content = read_file_or_panic(&PathBuf::from(svd_fname.clone()));

    let parsed = Document::parse(&content).expect("Failed to parse SVD content.");
    let registers = find_registers(&parsed, &reg_filter, &periph_filter, &syms_filter)
        .map_err(|positional| positional.with_fname(svd_fname))?;

    // If zero registers were chosen for generation, this run is useless.
    // Therefore we treat it as an error.
    // TODO: allow ignoring this error for special cases with a suitable flag on Config-struct
    if registers.is_empty() {
        return Err(Error::ZeroEntries);
    }

    info!("Found {} registers.", registers.len());
    Ok(registers)
}
