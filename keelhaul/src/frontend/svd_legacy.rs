use std::{
    collections::{hash_map::Entry, HashMap, HashSet},
    ops::RangeInclusive,
    path,
    str::FromStr,
};

use crate::{
    bit_count_to_rust_uint_type_str,
    error::{CommonParseError, Error, PositionalError, SvdParseError},
    model::{
        AddrRepr, ArchPtr, MakeAddrError, PtrSize, RegPath, Register, Registers, UniquePath,
        ValueOnReset,
    },
    util, Filters, IsAllowedOrBlocked, ItemFilter,
};
use itertools::Itertools;
use log::{info, warn};
use regex::Regex;
use roxmltree::Document;

struct XmlNode<'a, 'input>(pub roxmltree::Node<'a, 'input>);

trait IntoXmlNode<'a, 'input> {
    fn into_xml_node(self) -> XmlNode<'a, 'input>;
}

impl<'a, 'input> IntoXmlNode<'a, 'input> for roxmltree::Node<'a, 'input> {
    fn into_xml_node(self) -> XmlNode<'a, 'input> {
        XmlNode(self)
    }
}

impl<'a, 'input> XmlNode<'a, 'input> {
    fn find_text_by_tag_name(
        &self,
        tag: &str,
    ) -> Result<(&'a str, XmlNode<'a, 'a>), PositionalError<SvdParseError>> {
        self.maybe_find_text_by_tag_name(tag).ok_or_else(|| {
            SvdParseError::ExpectedTagInElement {
                elem_name: self.0.tag_name().name().to_owned(),
                tag: tag.to_owned(),
            }
            .with_byte_pos_range(self.0.range(), self.0.document())
        })
    }

    fn maybe_find_text_by_tag_name(&self, tag: &str) -> Option<(&'a str, XmlNode<'a, 'a>)> {
        self.0.children().find(|n| n.has_tag_name(tag)).map(|n| {
            (
                n.text().expect("node does not have text"),
                n.into_xml_node(),
            )
        })
    }

    fn children_with_tag_name(&self, tag: &str) -> Vec<XmlNode> {
        self.0
            .children()
            .filter(|n| n.has_tag_name(tag))
            .map(|n| n.into_xml_node())
            .collect_vec()
    }
}

/// Returns the appropriate multiplier for given character, represented by type parameter `P`
fn binary_size_mult_from_char<P: TryFrom<u64>>(c: char) -> Result<P, SvdParseError>
where
    SvdParseError: From<<P as TryFrom<u64>>::Error>,
{
    match c {
        'k' | 'K' => Ok(1024u64.try_into()?),
        'm' | 'M' => Ok((1024 * 1024u64).try_into()?),
        'g' | 'G' => Ok((1024 * 1024 * 1024u64).try_into()?),
        't' | 'T' => Ok((1024 * 1024 * 1024 * 1024u64).try_into()?),
        _ => Err(SvdParseError::InvalidSizeMultiplierSuffix(c)),
    }
}

#[test]
fn binary_size_mult_from_char_works() {
    // 32-bit
    assert_eq!(binary_size_mult_from_char('k'), Ok(1024u32));
    assert_eq!(binary_size_mult_from_char('m'), Ok(1024 * 1024u32));
    assert_eq!(binary_size_mult_from_char('g'), Ok(1024 * 1024 * 1024u32));
    assert!(binary_size_mult_from_char::<u32>('t').is_err());

    // 64-bit
    assert_eq!(binary_size_mult_from_char('k'), Ok(1024u64));
    assert_eq!(binary_size_mult_from_char('m'), Ok(1024 * 1024u64));
    assert_eq!(binary_size_mult_from_char('g'), Ok(1024 * 1024 * 1024u64));
    assert_eq!(
        binary_size_mult_from_char('t'),
        Ok(1024 * 1024 * 1024 * 1024u64)
    );
}

/// Parses an integer from `text`
///
/// This implementation is format aware and uses regex to ensure correct behavior.
fn parse_nonneg_int<P: ArchPtr>(text: &str) -> Result<P, SvdParseError>
where
    SvdParseError: From<<P as num::Num>::FromStrRadixErr>
        + From<<P as FromStr>::Err>
        + From<<P as TryFrom<u64>>::Error>,
{
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
        let number = P::from_str_radix(digits, 16)?;

        let size_mult = captures.get(2);
        (number, size_mult)
    } else if DEC_NONNEG_INT_RE.is_match(text) {
        // Safety: we checked above that at least one match exists in text
        let captures = DEC_NONNEG_INT_RE.captures_iter(text).next().unwrap();

        let digits = &captures[1];
        let number = digits.parse::<P>()?;

        let size_mult = captures.get(2);
        (number, size_mult)
    } else {
        return Err(SvdParseError::InvalidNonnegInt(text.to_owned()));
    };

    let size_mult: Option<P> = size_mult_capture
        // Safety: we know from the regex that there is only one possible size mult char
        .map(|s| s.as_str().chars().next().unwrap())
        .map(|c| binary_size_mult_from_char(c))
        .transpose()?;

    Ok(match size_mult {
        Some(mult) => number_part * mult,
        None => number_part,
    })
}

#[test]
fn parse_nonneg_int_works() {
    assert_eq!(
        parse_nonneg_int::<u32>("0xFFB00000").unwrap(),
        0xFFB0_0000u32
    );
    assert_eq!(
        parse_nonneg_int::<u32>("+0xFFB00000").unwrap(),
        0xFFB0_0000u32
    );
    // TODO: this test case is invalid. # means binary not hex, and the parser is faulty
    assert_eq!(
        parse_nonneg_int::<u32>("#FFB00000").unwrap(),
        0xFFB0_0000u32
    );
    assert_eq!(parse_nonneg_int::<u32>("42").unwrap(), 42u32);
    assert_eq!(parse_nonneg_int::<u32>("1 k").unwrap(), 1024u32);
    assert_eq!(
        parse_nonneg_int::<u32>("437260288").unwrap(),
        437_260_288u32
    );
}

#[derive(Clone, Default)]
struct RegPropGroupBuilder {
    /// Register bit-width.
    pub size: Option<u32>,
    /// Register access rights.
    pub access: Option<svd::Access>,
    /// Register value after reset.
    /// Actual reset value is calculated using reset value and reset mask.
    pub(crate) reset_value: Option<u64>,
    /// Register bits with defined reset value are marked as high.
    pub(crate) reset_mask: Option<u64>,
}

/// Add text position information to an [`SvdParseError`] converting it into a [`PositionalError`]
fn err_with_pos(e: impl Into<SvdParseError>, node: &XmlNode) -> PositionalError<SvdParseError> {
    e.into()
        .with_byte_pos_range(node.0.range(), node.0.document())
}

/// Finds a property from `node` by `tag`, calling `process` for its contents if present
///
/// Returns an error if value cannot be parsed in a legal way.
///
/// # Arguments
///
/// * `tag`     - The tag to locate within `node`
/// * `node`    - The node to search (does not recurse)
/// * `process` - The function to call for the found property
fn process_prop_from_node_if_present<T, F>(
    tag: &str,
    node: &XmlNode,
    process: F,
) -> Result<Option<T>, PositionalError<SvdParseError>>
where
    F: Fn(&str) -> Result<T, SvdParseError>,
{
    node.maybe_find_text_by_tag_name(tag)
        .map(|(s, prop_node)| process(s).map_err(|e| err_with_pos(e, &prop_node)))
        .transpose()
}

pub(crate) fn is_valid_bit_count(bit_count: u32) -> bool {
    matches!(bit_count, 8 | 16 | 32 | 64)
}

impl RegPropGroupBuilder {
    /// Returns a new [`RegPropGroupBuilder`] with applicable attributes from `node`
    ///
    /// Reads the following attributes from `node`:
    ///
    /// * size
    /// * access
    /// * resetValue
    /// * resetMask
    ///
    /// If a value was not available, the respective field is set to None.
    fn try_from_node(node: &XmlNode) -> Result<Self, PositionalError<SvdParseError>> {
        let mut properties = Self::default();
        properties.update_from_node(node)?;
        Ok(properties)
    }

    /// Inherit properties from parent and update with current node's properties if defined.
    ///
    /// # Arguments
    ///
    /// * `node` - can be either cluster or register node
    fn clone_and_update_from_node(
        &self,
        node: &XmlNode,
    ) -> Result<Self, PositionalError<SvdParseError>> {
        let mut properties = self.clone();
        properties.update_from_node(node)?;
        Ok(properties)
    }

    /// Update properties for this [`RegPropGroupBuilder`] where present
    ///
    /// # Arguments
    ///
    /// * `node` - can be either peripheral, cluster, or register node
    ///
    /// Updates the following attributes from `node`:
    ///
    /// * size
    /// * access
    /// * resetValue
    /// * resetMask
    fn update_from_node(&mut self, node: &XmlNode) -> Result<(), PositionalError<SvdParseError>> {
        if let Some(size) =
            process_prop_from_node_if_present("size", node, |s| Ok(s.parse::<u32>()?))?
        {
            self.size = Some(size);
        }
        if let Some(access) = process_prop_from_node_if_present("access", node, |s| {
            Ok(svd::Access::parse_str(s)
                .ok_or_else(|| CommonParseError::InvalidAccessType(s.to_owned()))?)
        })? {
            self.access = Some(access);
        };
        if let Some(reset_value) =
            process_prop_from_node_if_present("resetValue", node, parse_nonneg_int)?
        {
            self.reset_value = Some(reset_value);
        };
        if let Some(reset_mask) =
            process_prop_from_node_if_present("resetMask", node, parse_nonneg_int)?
        {
            self.reset_mask = Some(reset_mask);
        };
        Ok(())
    }

    pub(crate) fn build(
        self,
        reg_path: &str,
        default_register_size_bits: Option<u32>,
        default_reset_value: Option<u64>,
    ) -> Result<RegisterPropertiesGroup, SvdParseError> {
        let size = self.size.unwrap_or_else(|| {
            let size_bits = default_register_size_bits.expect("property 'size' was not defined and a default was not provided");
            warn!("property 'size' is not defined for register '{reg_path}' or any of its parents, assuming size = {size_bits}");
            assert!(is_valid_bit_count(size_bits));
            size_bits
        });
        let access = self.access.unwrap_or_else(|| {
            warn!("property 'access' is not defined for register '{reg_path}' or any of its parents, assuming access = read-write");
            svd::Access::ReadWrite
        });
        let reset_value = {
            // Unwrap: `size` was validated on construction using `PtrSize::is_valid_bit_count`
            let size = PtrSize::from_bit_count(size).unwrap();
            let reset_value = match self.reset_value {
                Some(value) => Some(value),
                None => {
                    if let Some(value) = default_reset_value {
                        warn!("property 'resetValue' is not defined for register '{reg_path}' or any of its parents, assuming resetValue = {:?}", value);
                        Some(value)
                    } else {
                        None
                    }
                }
            };
            let reset_mask = match self.reset_mask {
                Some(mask) => Some(mask),
                None => {
                    if default_reset_value.is_some() {
                        warn!("property 'resetMask' is not defined for register '{reg_path}' or any of its parents, assuming resetMask = {}::MAX", bit_count_to_rust_uint_type_str(size.count_bits()));
                        Some(size.max_value())
                    } else {
                        None
                    }
                }
            };
            // CMSIS-SVD requires both a reset value and a reset mask
            match (reset_value, reset_mask) {
                (Some(value), Some(mask)) => {
                    Some(ValueOnReset::new(value, Some(mask), size.count_bits()))
                }
                _ => None,
            }
        };

        Ok(RegisterPropertiesGroup::new(size, access, reset_value))
    }
}

#[derive(Clone, Copy)]
pub struct RegisterPropertiesGroup {
    /// Bit-width of register
    pub size: u32,
    /// Register access rights.
    pub access: svd::Access,
    /// Expected register value after reset based on source format
    ///
    /// Checking for the value may require special considerations in registers
    /// with read-only or write-only fields. These considerations are encoded in
    /// [ResetValue].
    pub(crate) reset_value: Option<ValueOnReset>,
}

impl RegisterPropertiesGroup {
    pub(crate) const fn new(
        size: u32,
        access: svd::Access,
        reset_value: Option<ValueOnReset>,
    ) -> Self {
        Self {
            size,
            access,
            reset_value,
        }
    }
}

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
    fn from_periph_node(periph_node: &XmlNode) -> Result<Self, PositionalError<SvdParseError>> {
        let (base_addr_str, base_addr_node) = periph_node.find_text_by_tag_name("baseAddress")?;
        let base_addr =
            parse_nonneg_int(base_addr_str).map_err(|e| err_with_pos(e, &base_addr_node))?;
        let (periph_name, _) = periph_node.find_text_by_tag_name("name")?;
        Ok(Self {
            periph_name: periph_name.to_string(),
            periph_base: base_addr,
            properties: RegPropGroupBuilder::try_from_node(periph_node)?,
            kind: RegisterParentKind::Periph,
        })
    }

    fn clone_and_update_from_cluster(
        &self,
        cluster_node: &XmlNode,
    ) -> Result<Self, PositionalError<SvdParseError>> {
        let cluster_name = cluster_node.find_text_by_tag_name("name")?.0.to_owned();
        let (cluster_offset_str, cluster_offset_node) =
            cluster_node.find_text_by_tag_name("addressOffset")?;
        let cluster_offset = parse_nonneg_int(cluster_offset_str)
            .map_err(|e| err_with_pos(e, &cluster_offset_node))?;

        Ok(Self {
            periph_name: self.periph_name.clone(),
            periph_base: self.periph_base,
            properties: self.properties.clone_and_update_from_node(cluster_node)?,
            kind: RegisterParentKind::Cluster {
                cluster_name,
                cluster_offset,
            },
        })
    }

    fn cluster_name(&self) -> Option<String> {
        match &self.kind {
            RegisterParentKind::Periph => None,
            RegisterParentKind::Cluster {
                cluster_name,
                cluster_offset: _,
            } => Some(cluster_name.clone()),
        }
    }

    fn cluster_address(&self) -> Option<u64> {
        match &self.kind {
            RegisterParentKind::Periph => None,
            RegisterParentKind::Cluster {
                cluster_name: _,
                cluster_offset,
            } => Some(*cluster_offset),
        }
    }
}

fn try_dim_element_from_xml_node(
    value: &XmlNode,
    lvl: svd::ValidateLevel,
) -> Result<svd::DimElement, PositionalError<SvdParseError>> {
    let dim = {
        let (dim, dim_node) = value.find_text_by_tag_name("dim")?;
        parse_nonneg_int(dim).map_err(|e| err_with_pos(e, &dim_node))?
    };
    let dim_increment = {
        let (dim_inc, dim_inc_node) = value.find_text_by_tag_name("dimIncrement")?;
        parse_nonneg_int(dim_inc).map_err(|e| err_with_pos(e, &dim_inc_node))?
    };
    let dim_index = value
            .maybe_find_text_by_tag_name("dimIndex")
            .and_then(|(index, _)| {
                let index = index.trim();
                let regex_numbered = Regex::new(r"^[0-9]+\-[0-9]+$").unwrap();
                let regex_lettered = Regex::new(r"^[A-Z]\-[A-Z]$").unwrap();
                let regex_listed = Regex::new(r"^[_0-9a-zA-Z]+(,\s*[_0-9a-zA-Z]+)+$").unwrap();
                let dim_index = if regex_numbered.is_match(index) {
                    let regex = Regex::new(r"^(?P<start>[0-9]+)\-(?P<end>[0-9]+)$").unwrap();
                    if let Some(captures) = regex.captures(index) {
                        // TODO: use error
                        let start: usize = captures
                            .name("start")
                            .expect("could not find dimension index range start")
                            .as_str()
                            .parse()
                            .unwrap();
                        // TODO: use error
                        let end: usize = captures
                            .name("end")
                            .expect("could not find dimension index range end")
                            .as_str()
                            .parse()
                            .unwrap();
                        warn!("dimIndex was present in input but was not constructed due to unfinished implementation; positions were {start} and {end}");
                        None
                    } else {
                        // TODO: use error
                        panic!("asd");
                    }
                } else if regex_lettered.is_match(index) {
                    let regex = Regex::new(r"").unwrap();
                    if let Some(captures) = regex.captures(index) {
                        // TODO: use error
                        let start: char =
                            captures.name("start").expect("").as_str().parse().unwrap();
                        // TODO: use error
                        let end: char = captures.name("end").expect("").as_str().parse().unwrap();
                        warn!("dimIndex was present in input but was not constructed due to unfinished implementation; positions were {start} and {end}");
                        None
                    } else {
                        // TODO: use error
                        panic!("asd");
                    }
                } else if regex_listed.is_match(index) {
                    let parts = index
                        .split(',')
                        .map(|s| s.trim())
                        .map(|s| s.to_owned())
                        .collect_vec();
                    Some(parts)
                } else {
                    // TODO: use error
                    // error: not supported dimIndex format: {}
                    panic!("asd");
                };
                dim_index
            });
    let dim_name = {
        value
            .maybe_find_text_by_tag_name("dimName")
            .map(|name| name.0.to_owned())
    };
    let dim_array_index = {
        // TODO: this is XML-element
        None
        /*
        let dim_array_index = match value.maybe_find_text_by_tag_name("dimArrayIndex") {
            Some(index) => {
                let a: u64 = parse_nonneg_int(index.0).map_err(|e| err_with_pos(e, &value))?;
                Some(a as usize)
            }
            None => None,
        };
        dim_array_index
        */
    };
    let builder = svd::DimElement::builder()
        .dim(dim)
        .dim_increment(dim_increment)
        .dim_index(dim_index)
        .dim_name(dim_name)
        .dim_array_index(dim_array_index);
    Ok(builder.build(lvl).unwrap())
}

fn check_node_count(
    node: &XmlNode,
    node_name: &str,
    vector: &[XmlNode],
    expected_count: RangeInclusive<usize>,
) -> Result<(), PositionalError<SvdParseError>> {
    let actual_count = vector.len();
    if expected_count.contains(&actual_count) {
        Ok(())
    } else {
        let node_name = node_name.to_owned();
        let error = SvdParseError::InvalidNodeCount {
            node_name,
            expected_count,
            actual_count,
        };
        Err(err_with_pos(error, node))
    }
}

fn process_registers(
    parent: &RegisterParent,
    register_node: &XmlNode,
    arch: PtrSize,
    reg_filter: Option<&ItemFilter<String>>,
    syms_regex: Option<&ItemFilter<String>>,
    default_reset_value: Option<u64>,
) -> Result<Option<Vec<Register>>, PositionalError<SvdParseError>> {
    let name = {
        let (name, _) = register_node.find_text_by_tag_name("name")?;
        name.to_owned()
    };
    let addr = {
        let address_base = parent.periph_base;
        let address_cluster = parent.cluster_address();
        let address_offset = {
            let (addr_offset_str, addr_offset_node) =
                register_node.find_text_by_tag_name("addressOffset")?;
            parse_nonneg_int(addr_offset_str).map_err(|e| err_with_pos(e, &addr_offset_node))?
        };
        addr_repr(
            address_base,
            address_cluster,
            address_offset,
            arch,
            name.clone(),
            register_node,
        )?
    };
    let mut registers = Vec::new();
    let dimensions =
        try_dim_element_from_xml_node(register_node, svd::ValidateLevel::Disabled).ok();
    if let Some(dimensions) = dimensions {
        // Found a list or an array of registers.
        for i in 0..dimensions.dim {
            // FIXME: verify that the array register's addresses are solved correctly.

            // Solve which string is used to replace the placeholder.
            let index = {
                if let Some(dim_index) = dimensions.dim_index.clone() {
                    // Index format is defined by the user.
                    dim_index.get(i as usize).unwrap().clone()
                } else {
                    // Index format is not defined by the user. Just use a number.
                    i.to_string()
                }
            };
            // Solve the name after replacing the placeholder.
            let subname = {
                // Choose which string to use as the base name.
                let dim_name = if let Some(dim_name) = dimensions.dim_name.clone() {
                    dim_name
                } else {
                    name.to_owned()
                };
                if dim_name.contains("%s") {
                    dim_name.replace("%s", &index)
                } else {
                    dim_name + &index
                }
            };
            // FIXME: block WITH brackets or WITOUT brackets? maybe both?
            // FIXME: we match against only the register's name, not the path. This is not a
            // great way to exclude registers. We should match against the entire path.
            if reg_filter.is_some_and(|f| f.is_blocked(&subname)) {
                info!("register {subname} is was not included due to values set in PATH_EXCLUDES");
                return Ok(None);
            }
            // Remove array brackets from the register name.
            let subname = subname.replace(['[', ']'], "");
            let path = {
                // Safety: max 3 levels of hierarchy (periph + cluster + reg)
                let path = unsafe {
                    RegPath::from_components(
                        parent.periph_name.clone(),
                        parent.cluster_name(),
                        subname.to_owned(),
                    )
                };
                let reg_path = path.join("-");
                if syms_regex.is_some_and(|f| f.is_blocked(&reg_path)) {
                    info!("Register {reg_path} was not included due to regex set in SYMS_REGEX");
                    return Ok(None);
                }
                path
            };
            let addr = {
                let (address_base, address_cluster, _) = addr.components();
                // TODO: use error
                let offset = i as u64 * dimensions.dim_increment as u64;
                addr_repr(
                    address_base,
                    address_cluster,
                    offset,
                    arch,
                    subname,
                    register_node,
                )?
            };
            let properties = {
                let reg_path = path.join("-");
                parent
                    .properties
                    .clone_and_update_from_node(register_node)?
                    .build(&reg_path, Some(arch.count_bits()), default_reset_value)
                    .map_err(|e| err_with_pos(e, register_node))?
            };
            let register = Register::new(
                path,
                addr,
                properties.size,
                properties.access,
                properties.reset_value,
            );
            registers.push(register);
        }
    } else {
        // Found a single register.
        let path = {
            // Safety: max 3 levels of hierarchy (periph + cluster + reg)
            let path = unsafe {
                RegPath::from_components(
                    parent.periph_name.clone(),
                    parent.cluster_name(),
                    name.to_owned(),
                )
            };
            let reg_path = path.join("-");
            if syms_regex.is_some_and(|f| f.is_blocked(&reg_path)) {
                info!("Register {reg_path} was not included due to regex set in SYMS_REGEX");
                return Ok(None);
            }
            path
        };
        // FIXME: we match against only the register's name, not the path. This is not a
        // great way to exclude registers. We should match against the entire path.
        if reg_filter.is_some_and(|f| f.is_blocked(&name)) {
            info!("register {name} is was not included due to values set in PATH_EXCLUDES");
            return Ok(None);
        }
        let properties = {
            let reg_path = path.join("-");
            parent
                .properties
                .clone_and_update_from_node(register_node)?
                .build(&reg_path, Some(arch.count_bits()), default_reset_value)
                .map_err(|e| err_with_pos(e, register_node))?
        };
        let register = Register::new(
            path,
            addr,
            properties.size,
            properties.access,
            properties.reset_value,
        );
        registers.push(register);
    }
    Ok(Some(registers))
}

fn addr_repr(
    base: u64,
    cluster: Option<u64>,
    offset: u64,
    arch: PtrSize,
    reg_name: String,
    register_node: &XmlNode,
) -> Result<AddrRepr, PositionalError<SvdParseError>> {
    AddrRepr::from_base_cluster_offset(base, cluster, offset, arch.count_bits()).map_err(|e| {
        err_with_pos(
            SvdParseError::ResolveAddr {
                reg_name: reg_name.clone(),
                inner: MakeAddrError {
                    id: Some(reg_name),
                    ..e
                },
            },
            register_node,
        )
    })
}

fn process_cluster(
    parent: &RegisterParent,
    cluster_node: &XmlNode,
    arch: PtrSize,
    reg_filter: Option<&ItemFilter<String>>,
    syms_regex: Option<&ItemFilter<String>>,
    default_reset_value: Option<u64>,
) -> Result<Option<Vec<Register>>, PositionalError<SvdParseError>> {
    let current_parent = parent.clone_and_update_from_cluster(cluster_node)?;
    let mut cluster_registers = Vec::new();
    for register_node in cluster_node.children_with_tag_name("register") {
        if let Some(registers) = process_registers(
            &current_parent,
            &register_node,
            arch,
            reg_filter,
            syms_regex,
            default_reset_value,
        )? {
            cluster_registers.extend(registers);
        }
    }
    Ok(Some(cluster_registers))
}

fn process_peripheral(
    periph_node: XmlNode,
    arch: PtrSize,
    filters: &Filters,
    default_reset_value: Option<u64>,
) -> Result<Option<Vec<Register>>, PositionalError<SvdParseError>> {
    let periph = RegisterParent::from_periph_node(&periph_node)?;
    let periph_name = &periph.periph_name;

    if filters
        .top
        .as_ref()
        .is_some_and(|f| f.is_blocked(&periph_name.to_lowercase()))
    {
        info!("Peripheral {periph_name} was not included due to values set in INCLUDE_PERIPHERALS and/or EXCLUDE_PERIPHERALS");
        return Ok(None);
    }

    let registers_nodes = periph_node.children_with_tag_name("registers");
    check_node_count(&periph_node, "registers", &registers_nodes, 1..=1)?;
    let registers_node = registers_nodes.first().unwrap();

    let mut peripheral_registers = Vec::new();
    for cluster_node in registers_node.children_with_tag_name("cluster") {
        if let Some(registers) = process_cluster(
            &periph,
            &cluster_node,
            arch,
            filters.reg.as_ref(),
            filters.path_regex.as_ref(),
            default_reset_value,
        )? {
            peripheral_registers.extend(registers);
        }
    }
    for register_node in registers_node.children_with_tag_name("register") {
        if let Some(registers) = process_registers(
            &periph,
            &register_node,
            arch,
            filters.reg.as_ref(),
            filters.path_regex.as_ref(),
            default_reset_value,
        )? {
            peripheral_registers.extend(registers);
        }
    }
    Ok(Some(peripheral_registers))
}

fn process_peripherals(
    peripherals_node: &XmlNode,
    arch: PtrSize,
    filters: &Filters,
    default_reset_value: Option<u64>,
) -> Result<Vec<Register>, PositionalError<SvdParseError>> {
    let mut registers = Vec::new();
    for peripheral_node in peripherals_node.children_with_tag_name("peripheral") {
        if let Some(peripheral_registers) =
            process_peripheral(peripheral_node, arch, filters, default_reset_value)?
        {
            registers.extend(peripheral_registers);
        }
    }
    Ok(registers)
}

fn process_device(
    device_node: &XmlNode,
    arch: PtrSize,
    filters: &Filters,
    default_reset_value: Option<u64>,
) -> Result<Vec<Register>, PositionalError<SvdParseError>> {
    let peripherals_nodes = device_node.children_with_tag_name("peripherals");
    check_node_count(device_node, "peripherals", &peripherals_nodes, 1..=1)?;
    let peripherals_node = peripherals_nodes.first().unwrap();
    process_peripherals(peripherals_node, arch, filters, default_reset_value)
}

fn process_root(
    root_node: XmlNode,
    arch: PtrSize,
    filters: &Filters,
    default_reset_value: Option<u64>,
) -> Result<Vec<Register>, PositionalError<SvdParseError>> {
    let device_nodes = root_node.children_with_tag_name("device");
    check_node_count(&root_node, "device", &device_nodes, 1..=1)?;
    let device_node = device_nodes.first().unwrap();
    process_device(device_node, arch, filters, default_reset_value)
}

/// Find registers from SVD XML-document.
fn find_registers(
    parsed: &Document,
    arch: PtrSize,
    filters: &Filters,
    default_reset_value: Option<u64>,
) -> Result<Registers, PositionalError<SvdParseError>> {
    let root = parsed.root().into_xml_node();
    let registers = process_root(root, arch, filters, default_reset_value)?;
    let mut peripherals = HashSet::new();
    let mut addresses = HashMap::new();
    for register in &registers {
        peripherals.insert(register.top_container_name());
        let addr = register.addr();
        if let Entry::Vacant(entry) = addresses.entry(addr) {
            entry.insert(register.path().join("-"));
        } else {
            let reg2 = addresses
                .get(&addr)
                .expect("failed to find register name by key");
            warn!("Address for register {reg}@{addr:#x?} is already registered for another register {reg2}. {reg} is ignored.",
                reg = register.path().join("-"));
        }
    }

    info!("Found {} peripherals:", peripherals.len());
    for peripheral in peripherals {
        info!("    {peripheral}");
    }
    Ok(registers.into())
}

/// Parse the file at `svd_path` into a list of registers with provided filters & constraints
///
/// # Arguments
///
/// * `svd_path`        - The path to the SVD file
/// * `arch`            - The size used by the architecture to represent addresses
/// * `reg_filter`      - What registers to include or exclude
/// * `periph_filter`   - What peripherals to include or exclude
/// * `syms_filter` -   - What symbols to include or exclude (applying to full register identifier)
///
/// # Safety
///
/// May work incorrectly for CMSIS-SVD v1.3 or above, where the maximum of 3 levels of hierarchy can
/// be surpassed.
pub(crate) unsafe fn parse_svd_into_registers(
    svd_source: &path::Path,
    arch: PtrSize,
    filters: &Filters,
    default_reset_value: Option<u64>,
) -> Result<Registers, Error> {
    let svd_content = util::read_file_or_panic(svd_source);
    let parsed = Document::parse(&svd_content).expect("Failed to parse SVD content.");
    let registers = find_registers(&parsed, arch, filters, default_reset_value)
        .map_err(|positional| positional.with_fname(format!("{}", svd_source.display())))
        .map_err(Box::new)?;

    // If zero registers were chosen for generation, this run is useless.
    // Therefore we treat it as an error.
    // TODO: allow ignoring this error for special cases with a suitable flag on Config-struct
    if registers.is_empty() {
        return Err(Error::ZeroEntries);
    }

    info!("Found {} registers.", registers.len());
    Ok(registers)
}
