//! SVD-file parser for register test generator.

use crate::{
    validate_path_existence, Access, AddrOverflowError, AddrRepr, CommonParseError, Error,
    NotImplementedError, PtrWidth, RegPath, Register, Registers, SvdParseError,
};
use itertools::Itertools;
use log::{info, warn};
use regex::Regex;
use roxmltree::{Document, Node};
use std::{
    collections::{hash_map::Entry, HashMap},
    env,
    fs::read_to_string,
    panic,
    path::PathBuf,
};

/// Try to extract path to excludes-file from environment variable.
fn read_excludes_path_from_env() -> Option<PathBuf> {
    if let Ok(path_str) = env::var("PATH_EXCLUDES") {
        let path = validate_path_existence(&path_str);
        Some(path)
    } else {
        None
    }
}

/// Try to get names of excluded registers.
fn read_excludes_from_env() -> Option<Vec<String>> {
    let path_excludes = read_excludes_path_from_env();
    match path_excludes {
        Some(path) => {
            let content = read_to_string(path).expect("Failed to read excludes content.");
            let registers = content
                .split('\n')
                .into_iter()
                .map(ToOwned::to_owned)
                .collect_vec();
            Some(registers)
        }
        None => None,
    }
}

/// What items of type `T` are allowed or not
enum ItemFilter<T: PartialEq> {
    List {
        // If set, only the specified items are allowed. If not set, all items are
        // allowed except the ones listed in blocklist.
        white_list: Option<Vec<T>>,
        // These items are always blocked even if present in `white_list`
        block_list: Vec<T>,
    },
    Regex(Regex),
}

impl<T: PartialEq> ItemFilter<T> {
    fn list(white_list: Option<Vec<T>>, block_list: Vec<T>) -> ItemFilter<T> {
        Self::List {
            white_list,
            block_list,
        }
    }

    fn regex(regex: Regex) -> ItemFilter<T> {
        Self::Regex(regex)
    }

    fn is_allowed(&self, value: &T) -> bool
    where
        T: AsRef<str>,
    {
        match self {
            ItemFilter::List {
                white_list,
                block_list,
            } => {
                // Items in block list are always blocked
                if block_list.contains(value) {
                    return false;
                }

                match &white_list {
                    Some(white_list) => white_list.contains(value),
                    None => true,
                }
            }
            ItemFilter::Regex(re) => re.is_match(value.as_ref()),
        }
    }

    fn is_blocked(&self, value: &T) -> bool
    where
        T: AsRef<str>,
    {
        !self.is_allowed(value)
    }
}

/// Read an environment variable into a Vec<String>
///
/// # Parameters:
///
/// `var` - The name of the environment variable
/// `sep` - The separator for Vec elements
///
/// Returns Some(`v`) if the variable is present, None otherwise
fn read_vec_from_env(var: &str, sep: char) -> Option<Vec<String>> {
    if let Ok(included_str) = env::var(var) {
        let peripherals = included_str.split(sep).map(ToOwned::to_owned).collect_vec();
        // TODO: validate that these are valid peripherals
        Some(peripherals)
    } else {
        None
    }
}

/// Read the input SVD to string
fn read_input_svd_to_string() -> String {
    let svd_path = env::var("PATH_SVD").unwrap_or_else(|_| panic!("PATH_SVD must be set"));
    let svd_path = PathBuf::from(svd_path);
    if !svd_path.exists() {
        panic!("SVD was not found at {}", svd_path.display());
    }
    read_to_string(svd_path).unwrap()
}

/// Find a child node with given tag name.
fn find_text_in_node_by_tag_name<'a>(node: &'a Node, tag: &str) -> Result<&'a str, SvdParseError> {
    maybe_find_text_in_node_by_tag_name(node, tag).ok_or(SvdParseError::ExpectedTagInElement {
        elem_name: node.tag_name().name().to_owned(),
        tag: tag.to_owned(),
    })
}

/// Try to find a child node with given name.
fn maybe_find_text_in_node_by_tag_name<'a>(node: &'a Node, tag: &str) -> Option<&'a str> {
    node.children()
        .find(|n| n.has_tag_name(tag))
        .map(|n| n.text().expect("Node does not have text."))
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

// The presence of this pattern in the register name likely indicates that this
// is an array register
//
// TODO: should use a more robust way of detecting arrays, i.e., checking the
// fields for the reg in question
const SVD_ARRAY_REPETITION_PATTERN: &str = "%s";

/// Find registers from SVD XML-document.
fn find_registers(
    parsed: &Document,
    reg_filter: &ItemFilter<String>,
    periph_filter: &ItemFilter<String>,
    syms_regex: &Option<ItemFilter<String>>,
) -> Result<Registers<u32>, SvdParseError> {
    let mut peripherals = Vec::new();
    let mut registers = Vec::new();
    let mut addresses = HashMap::new();
    let peripheral_nodes = parsed
        .descendants()
        .filter(|n| n.has_tag_name("peripheral"));

    for peripheral_node in peripheral_nodes {
        let base_address_str = find_text_in_node_by_tag_name(&peripheral_node, "baseAddress")?;
        let base_addr = parse_nonneg_int_u64(base_address_str)?;
        let peripheral_name = find_text_in_node_by_tag_name(&peripheral_node, "name")?.to_owned();
        peripherals.push(peripheral_name.to_owned());

        if periph_filter.is_blocked(&peripheral_name.to_lowercase()) {
            info!("Peripheral {peripheral_name} was not included due to values set in INCLUDE_PERIPHERALS and/or EXCLUDE_PERIPHERALS");
            continue;
        }

        for cluster in peripheral_node
            .descendants()
            .filter(|n| n.has_tag_name("cluster"))
        {
            let address_offset_cluster_str =
                find_text_in_node_by_tag_name(&cluster, "addressOffset")?;
            let cluster_addr_offset = parse_nonneg_int_u64(address_offset_cluster_str)?;
            let cluster_name = find_text_in_node_by_tag_name(&cluster, "name")?.to_owned();
            for register in cluster.descendants().filter(|n| n.has_tag_name("register")) {
                let reg_name = find_text_in_node_by_tag_name(&register, "name")?.to_string();

                //let reg_name = remove_illegal_characters(reg_name);
                let path = RegPath::from_components(
                    peripheral_name.clone(),
                    Some(cluster_name.clone()),
                    reg_name.clone(),
                );
                let reg_path = path.join("-");

                if let Some(syms_regex) = syms_regex {
                    if syms_regex.is_blocked(&reg_path) {
                        info!(
                            "Register {reg_path} was not included due to regex set in SYMS_REGEX"
                        );
                        continue;
                    }
                }

                // FIXME: we match against only the register's name, not the path. This is not a
                // great way to exclude registers. We should match against the entire path.
                if reg_filter.is_blocked(&reg_name.to_string()) {
                    info!("register {reg_name} is was not included due to values set in PATH_EXCLUDES");
                    continue;
                }

                if reg_name.contains(SVD_ARRAY_REPETITION_PATTERN) {
                    warn!("{}, skipping", NotImplementedError::SvdArray(reg_path));
                    continue;
                }

                let value_reset_str = find_text_in_node_by_tag_name(&register, "resetValue")?;
                let reset_val = parse_nonneg_int_u64(value_reset_str)?;
                let address_offset_register_str =
                    find_text_in_node_by_tag_name(&register, "addressOffset")?;
                let reg_addr_offset = parse_nonneg_int_u64(address_offset_register_str)?;
                let access = Access::from_svd_access_type(maybe_find_text_in_node_by_tag_name(&register, "access").unwrap_or_else(|| {
                    warn!("register {} does not have access type. Access type is assumed to be 'read-write'.", reg_path);
                    "read-write"
                }))?;
                let size_str = find_text_in_node_by_tag_name(&register, "size")?;
                let size: u64 = size_str.parse()?;
                let size = match PtrWidth::from_bit_count(size) {
                    Some(size) => size,
                    None => {
                        return Err(SvdParseError::BitCountToPtrWidth(size));
                    }
                };

                let full_address = base_addr + cluster_addr_offset + reg_addr_offset;
                let addr = AddrRepr::<u64>::Comps {
                    base: base_addr,
                    // ???: cluster assumed to always exist
                    cluster: Some(cluster_addr_offset),
                    offset: reg_addr_offset,
                };
                let addr = AddrRepr::<u32>::try_from(addr.clone()).map_err(|_| {
                    SvdParseError::AddrOverflow(AddrOverflowError(path.join("-"), addr.clone()))
                })?;
                if let Entry::Vacant(entry) = addresses.entry(full_address) {
                    entry.insert(reg_name.clone());
                    let register = Register {
                        // ???: reset value assumed to always exist
                        reset_val: Some(reset_val),
                        // ???: I noticed that cluster is assumed to always exist even though it's
                        // optional in the data model. Wrap in Some for now and expect breakage
                        // somewhere prior to this line.
                        path,
                        addr,
                        access,
                        size,
                    };
                    registers.push(register);
                } else {
                    let address_holder = addresses
                        .get(&full_address)
                        .expect("failed to find register name by key");
                    warn!("register {reg_name}'s full address is already taken by register {address_holder}. This register is ignored.");
                }
            }
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
    let syms_regex = env::var("SYMS_REGEX")
        .ok()
        .map(|s| Regex::new(&s))
        .transpose()?
        .map(ItemFilter::regex);
    let periph_filter =
        ItemFilter::list(include_peripherals, exclude_peripherals.unwrap_or(vec![]));
    let reg_filter = ItemFilter::list(None, read_excludes_from_env().unwrap_or(vec![]));
    let content = read_input_svd_to_string();
    let parsed = Document::parse(&content).expect("Failed to parse SVD content.");
    let registers = find_registers(&parsed, &reg_filter, &periph_filter, &syms_regex)?;
    info!("Found {} registers.", registers.len());
    Ok(registers)
}

impl Access {
    /// Implements parsing access type as specified by CMSIS-SVD schema
    pub fn from_svd_access_type(s: &str) -> Result<Self, CommonParseError> {
        match s {
            "read-only" => Ok(Access::ReadOnly),
            "write-only" => Ok(Access::WriteOnly),
            "read-write" => Ok(Access::ReadWrite),
            "writeOnce" => Ok(Access::WriteOnce),
            "read-writeOnce" => Ok(Access::ReadWriteOnce),
            _ => Err(CommonParseError::InvalidAccessType(s.to_owned())),
        }
    }
}
