//! SVD-file parser for register test generator.

use crate::{
    get_or_create, model::*, validate_path_existence, Access, NotImplementedError, ParseError,
    PtrWidth, Registers,
};
use itertools::Itertools;
use log::{info, warn};
use regex::Regex;
use roxmltree::{Document, Node};
use std::{
    collections::{hash_map::Entry, HashMap},
    env,
    fs::{self, read_to_string, File},
    io::Write,
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
struct ItemFilter<T: PartialEq> {
    // If set, only the specified items are allowed. If not set, all items are
    // allowed except the ones listed in blocklist.
    white_list: Option<Vec<T>>,
    // These items are always blocked even if present in `white_list`
    block_list: Vec<T>,
}

impl<T: PartialEq> ItemFilter<T> {
    fn new(white_list: Option<Vec<T>>, block_list: Vec<T>) -> ItemFilter<T> {
        Self {
            white_list,
            block_list,
        }
    }

    fn is_allowed(&self, value: &T) -> bool {
        // Items in block list are always blocked
        if self.block_list.contains(value) {
            return false;
        }

        match &self.white_list {
            Some(white_list) => white_list.contains(value),
            None => true,
        }
    }

    fn is_blocked(&self, value: &T) -> bool {
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

pub const PARSED_FILENAME: &str = "parsed.json";

/// Extract path to output file from environment variable.
/// Get handle to output file.
fn open_output_file() -> File {
    // Safety: OUT_DIR always exists
    let out_dir = env::var("OUT_DIR").unwrap();
    let path_str = format!("{out_dir}/{PARSED_FILENAME}");
    let path = get_or_create(&path_str);
    fs::OpenOptions::new()
        .create(true)
        .write(true)
        .truncate(true)
        .open(path)
        .expect("Failed to open output file.")
}

/// Find a child node with given tag name.
fn find_text_in_node_by_tag_name<'a>(node: &'a Node, tag: &str) -> Result<&'a str, ParseError> {
    maybe_find_text_in_node_by_tag_name(node, tag).ok_or(ParseError::ExpectedTag(tag.to_owned()))
}

/// Try to find a child node with given name.
fn maybe_find_text_in_node_by_tag_name<'a>(node: &'a Node, tag: &str) -> Option<&'a str> {
    node.children()
        .find(|n| n.has_tag_name(tag))
        .map(|n| n.text().expect("Node does not have text."))
}

trait ArchUsize<U> {
    fn from_str_radix(digits: &str, radix: u32) -> Result<U, ParseError>;
}

impl ArchUsize<u32> for u32 {
    fn from_str_radix(digits: &str, radix: u32) -> Result<u32, ParseError> {
        u32::from_str_radix(digits, radix).map_err(ParseError::from)
    }
}

impl ArchUsize<u64> for u64 {
    fn from_str_radix(digits: &str, radix: u32) -> Result<u64, ParseError> {
        u64::from_str_radix(digits, radix).map_err(ParseError::from)
    }
}

fn binary_size_mult_from_char(c: char) -> Result<u64, ParseError> {
    match c {
        'k' | 'K' => Ok(1024),
        'm' | 'M' => Ok(1024 * 1024),
        'g' | 'G' => Ok(1024 * 1024 * 1024),
        't' | 'T' => Ok(1024 * 1024 * 1024 * 1024),
        _ => Err(ParseError::InvalidSizeMultiplierSuffix(c)),
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
fn parse_nonneg_int_u64(text: &str) -> Result<u64, ParseError> {
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
        return Err(ParseError::InvalidInt(text.to_owned()));
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

fn get_register_properties(path: String, node: Node) -> Result<RegisterProperties, ParseError> {
    let size = match find_text_in_node_by_tag_name(&node, "size") {
        Ok(size_str) => {
            let size: u64 = size_str.parse()?;
            Some(PtrWidth::from_bit_count(size)?)
        }
        Err(_) => None,
    };
    let access = Access::from_svd_access_type(
        maybe_find_text_in_node_by_tag_name(&node, "access").unwrap_or_else(|| {
            warn!(
                "register {path} does not have access type. Access type is assumed to be 'read-write'."
            );
            "read-write"
        }),
    )?;
    let reset_value =
        if let Some(value_str) = maybe_find_text_in_node_by_tag_name(&node, "resetValue") {
            Some(parse_nonneg_int_u64(value_str)?)
        } else {
            None
        };
    let reset_mask =
        if let Some(value_str) = maybe_find_text_in_node_by_tag_name(&node, "resetMask") {
            Some(parse_nonneg_int_u64(value_str)?)
        } else {
            None
        };
    Ok(RegisterProperties {
        size,
        access,
        reset_value,
        reset_mask,
    })
}

fn process_register(
    node: Node,
    reg_filter: &ItemFilter<String>,
    path_parent: Vec<String>,
) -> Result<Option<ProcessedRegister>, ParseError> {
    let name = find_text_in_node_by_tag_name(&node, "name")?.to_string();
    let addr_offset_str = find_text_in_node_by_tag_name(&node, "addressOffset")?;
    let addr_offset = parse_nonneg_int_u64(addr_offset_str)?;

    let mut path = path_parent.clone();
    path.push(name.clone());
    let path = path.join("-");

    if reg_filter.is_blocked(&name) {
        info!("register {name} is was not included due to values set in PATH_EXCLUDES");
        return Ok(None);
    }

    if name.contains(SVD_ARRAY_REPETITION_PATTERN) {
        warn!("{}, skipping", NotImplementedError::SvdArray(path));
        return Ok(None);
    }

    let properties = get_register_properties(path, node)?;

    let register = ProcessedRegister {
        name,
        addr_offset,
        properties,
    };
    Ok(Some(register))
}

fn process_cluster(
    node: Node,
    reg_filter: &ItemFilter<String>,
    path_parent: Vec<String>,
) -> Result<Option<ProcessedCluster>, ParseError> {
    let name = find_text_in_node_by_tag_name(&node, "name")?.to_owned();
    let addr_offset_str = find_text_in_node_by_tag_name(&node, "addressOffset")?;
    let addr_offset = parse_nonneg_int_u64(addr_offset_str)?;

    let mut registers = Vec::new();
    for register_node in node.descendants().filter(|n| n.has_tag_name("register")) {
        if let Some(register) = process_register(register_node, reg_filter, path_parent.clone())? {
            registers.push(register);
        }
    }

    let cluster = ProcessedCluster {
        name,
        addr_offset,
        registers,
    };
    Ok(Some(cluster))
}

fn process_peripheral(
    node: Node,
    periph_filter: &ItemFilter<String>,
    reg_filter: &ItemFilter<String>,
) -> Result<Option<ProcessedPeripheral>, ParseError> {
    let name = find_text_in_node_by_tag_name(&node, "name")?.to_owned();
    let base_addr_str = find_text_in_node_by_tag_name(&node, "baseAddress")?;
    let base_addr = parse_nonneg_int_u64(base_addr_str)?;

    if periph_filter.is_blocked(&name.to_lowercase()) {
        info!("Peripheral {name} was not included due to values set in INCLUDE_PERIPHERALS and/or EXCLUDE_PERIPHERALS");
        return Ok(None);
    }

    let mut clusters = Vec::new();
    let mut registers = Vec::new();
    let path = vec![name.clone()];

    let registers_nodes = node
        .children()
        .filter(|n| n.has_tag_name("registers"))
        .collect_vec();
    assert!(
        registers_nodes.len() == 1,
        "Peripheral-node must have one registers-node."
    );
    let registers_node = registers_nodes.into_iter().take(1).next().unwrap();

    // Process clusters.
    for cluster_node in registers_node
        .children()
        .filter(|n| n.has_tag_name("cluster"))
    {
        if let Some(cluster) = process_cluster(cluster_node, reg_filter, path.clone())? {
            clusters.push(cluster);
        }
    }

    // Process bare registers.
    for register_node in registers_node
        .children()
        .filter(|n| n.has_tag_name("register"))
    {
        if let Some(register) = process_register(register_node, reg_filter, path.clone())? {
            registers.push(register);
        }
    }

    let peripheral = ProcessedPeripheral {
        name,
        base_addr,
        registers,
        clusters,
    };
    Ok(Some(peripheral))
}

/// Find registers from SVD XML-document.
fn find_registers(
    parsed: &Document,
    reg_filter: &ItemFilter<String>,
    periph_filter: &ItemFilter<String>,
) -> Result<Registers, ParseError> {
    let mut peripherals = Vec::new();
    let nodes = parsed
        .descendants()
        .filter(|n| n.has_tag_name("peripheral"));
    for node in nodes {
        if let Some(peripheral) = process_peripheral(node, periph_filter, reg_filter)? {
            peripherals.push(peripheral);
        }
    }
    info!("Found {} peripherals:", peripherals.len());

    let mut registers: Vec<Box<dyn IsRegister>> = Vec::new();
    for peripheral in peripherals {
        info!("    {} 0x{:x}", peripheral.name, peripheral.base_addr);
        for register in &peripheral.registers {
            registers.push(Box::new(BareRegister {
                peripheral: peripheral.clone(),
                register: register.clone(),
            }));
        }
        for cluster in &peripheral.clusters {
            for register in &cluster.registers {
                registers.push(Box::new(RegisterUnderCluster {
                    peripheral: peripheral.clone(),
                    cluster: cluster.clone(),
                    register: register.clone(),
                }));
            }
        }
    }
    let mut addresses = HashMap::new();
    for register in &registers {
        // FIXME: unwrap
        let full_address = register.full_address().unwrap();
        let full_path = register.full_path("-");

        if let Entry::Vacant(entry) = addresses.entry(full_address) {
            entry.insert(full_path.clone());
        } else {
            let address_holder = addresses
                .get(&full_address)
                .expect("failed to find register name by key");
            warn!("register {full_path}'s full address is already taken by register {address_holder}. This register is ignored.");
        }
    }

    Ok(registers.into())
}

/// Write found registers to output file.
fn write_output(registers: &[Box<dyn IsRegister>], file: &mut File) {
    let registers_as_hashmaps = registers.iter().map(|r| r.to_hashmap()).collect_vec();
    let output = json::stringify(registers_as_hashmaps);
    file.write_all(output.as_bytes())
        .expect("Failed to write to output file.");
}

/// Parse SVD-file.
pub fn parse() {
    let include_peripherals = read_vec_from_env("INCLUDE_PERIPHERALS", ',');
    let exclude_peripherals = read_vec_from_env("EXCLUDE_PERIPHERALS", ',');
    let periph_filter = ItemFilter::new(include_peripherals, exclude_peripherals.unwrap_or(vec![]));
    let reg_filter = ItemFilter::new(None, read_excludes_from_env().unwrap_or(vec![]));
    let content = read_input_svd_to_string();
    let parsed = Document::parse(&content).expect("Failed to parse SVD content.");
    let registers = find_registers(&parsed, &reg_filter, &periph_filter).unwrap();
    info!("Found {} registers.", registers.len());

    let mut file_output = open_output_file();
    write_output(&registers, &mut file_output);
}

impl Access {
    /// Implements parsing access type as specified by CMSIS-SVD schema
    pub fn from_svd_access_type(s: &str) -> Result<Self, ParseError> {
        match s {
            "read-only" => Ok(Access::ReadOnly),
            "write-only" => Ok(Access::WriteOnly),
            "read-write" => Ok(Access::ReadWrite),
            "writeOnce" => Ok(Access::WriteOnce),
            "read-writeOnce" => Ok(Access::ReadWriteOnce),
            _ => Err(ParseError::InvalidAccessType(s.to_owned())),
        }
    }
}
