//! SVD-file parser for register test generator.

use itertools::Itertools;
use log::{info, warn};
use regex::Regex;
use register_selftest_generator_common::{get_or_create, validate_path_existence, Register};
use roxmltree::{Document, Node};
use std::{
    collections::{hash_map::Entry, HashMap},
    env,
    fs::{self, read_to_string, File},
    io::Write,
    num::ParseIntError,
    panic,
    path::PathBuf,
};
use thiserror::Error;

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
                .map(remove_illegal_characters)
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

#[derive(Error, Debug)]
enum Error {
    #[error("expected field in node: {0}")]
    ExpectedTag(String),
    #[error("could not parse int")]
    ParseInt(#[from] ParseIntError),
    #[error("expected int: {0}")]
    InvalidInt(String),
    #[error("invalid size multiplier suffix: {0}")]
    InvalidSizeMultiplierSuffix(char),
    #[error("invalid access type: {0}")]
    InvalidAccessType(String),
}

/// Find a child node with given tag name.
fn find_text_in_node_by_tag_name<'a>(node: &'a Node, tag: &str) -> Result<&'a str, Error> {
    maybe_find_text_in_node_by_tag_name(node, tag).ok_or(Error::ExpectedTag(tag.to_owned()))
}

/// Try to find a child node with given name.
fn maybe_find_text_in_node_by_tag_name<'a>(node: &'a Node, tag: &str) -> Option<&'a str> {
    node.children()
        .find(|n| n.has_tag_name(tag))
        .map(|n| n.text().expect("Node does not have text."))
}

/// Remove illegal characters from register name.
///
/// These characters will be put into test case names, and thus need to be removed.
fn remove_illegal_characters(name: &str) -> String {
    let mut name_new = name.to_owned();
    let illegals = ['(', ')', '[', ']', '%'];
    let mut found_illegals = Vec::new();
    for illegal in illegals {
        if name_new.contains(illegal) {
            found_illegals.push(illegal);
            name_new = name_new.replace(illegal, "_");
        }
    }
    if !found_illegals.is_empty() {
        let symbols = found_illegals
            .iter()
            .map(|c| format!("\"{}\"", c.to_owned()))
            .join(", ");
        warn!(
            "Register {}'s name contains {} illegal characters: {}. These characters are replaced with underscores ('_').",
            name,
            found_illegals.len(),
            symbols
        );
    }
    name_new
}

trait ArchUsize<U> {
    fn from_str_radix(digits: &str, radix: u32) -> Result<U, Error>;
}

impl ArchUsize<u32> for u32 {
    fn from_str_radix(digits: &str, radix: u32) -> Result<u32, Error> {
        u32::from_str_radix(digits, radix).map_err(Error::from)
    }
}

impl ArchUsize<u64> for u64 {
    fn from_str_radix(digits: &str, radix: u32) -> Result<u64, Error> {
        u64::from_str_radix(digits, radix).map_err(Error::from)
    }
}

fn binary_size_mult_from_char(c: char) -> Result<u64, Error> {
    match c {
        'k' | 'K' => Ok(1024),
        'm' | 'M' => Ok(1024 * 1024),
        'g' | 'G' => Ok(1024 * 1024 * 1024),
        't' | 'T' => Ok(1024 * 1024 * 1024 * 1024),
        _ => Err(Error::InvalidSizeMultiplierSuffix(c)),
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
fn parse_nonneg_int_u64(text: &str) -> Result<u64, Error> {
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
        return Err(Error::InvalidInt(text.to_owned()));
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

/// Software access rights e.g., read-only or read-write, as defined by
/// CMSIS-SVD `accessType`.
enum Access {
    /// read-only
    ReadOnly,
    /// write-only
    WriteOnly,
    /// read-write
    ReadWrite,
    /// writeOnce
    WriteOnce,
    /// read-writeOnce
    ReadWriteOnce,
}

impl Access {
    fn from_svd_access_type(s: &str) -> Result<Self, Error> {
        match s {
            "read-only" => Ok(Access::ReadOnly),
            "write-only" => Ok(Access::WriteOnly),
            "read-write" => Ok(Access::ReadWrite),
            "writeOnce" => Ok(Access::WriteOnce),
            "read-writeOnce" => Ok(Access::ReadWriteOnce),
            _ => Err(Error::InvalidAccessType(s.to_owned())),
        }
    }

    fn is_read(&self) -> bool {
        match self {
            Access::ReadOnly | Access::ReadWrite => true,
            Access::WriteOnly => false,
            Access::WriteOnce => {
                warn!("a field uses write-once, assuming not readable");
                false
            }
            Access::ReadWriteOnce => {
                warn!("a field uses read-write-once, assuming readable");
                true
            }
        }
    }

    fn is_write(&self) -> bool {
        match self {
            Access::ReadOnly => false,
            Access::WriteOnly | Access::ReadWrite | Access::WriteOnce | Access::ReadWriteOnce => {
                true
            }
        }
    }
}

/// Find registers from SVD XML-document.
fn find_registers(
    parsed: &Document,
    reg_filter: &ItemFilter<String>,
    periph_filter: &ItemFilter<String>,
) -> Result<Vec<Register>, Error> {
    let mut peripherals = Vec::new();
    let mut registers = Vec::new();
    let mut addresses = HashMap::new();
    let peripheral_nodes = parsed
        .descendants()
        .filter(|n| n.has_tag_name("peripheral"));

    for peripheral_node in peripheral_nodes {
        let base_address_str = find_text_in_node_by_tag_name(&peripheral_node, "baseAddress")?;
        let base_address = parse_nonneg_int_u64(base_address_str)?;
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
            let address_offset_cluster = parse_nonneg_int_u64(address_offset_cluster_str)?;
            let name_cluster = find_text_in_node_by_tag_name(&cluster, "name")?.to_owned();
            for register in cluster.descendants().filter(|n| n.has_tag_name("register")) {
                let name = find_text_in_node_by_tag_name(&register, "name")?;
                let name_register = remove_illegal_characters(name);
                if reg_filter.is_blocked(&name.to_string()) {
                    info!("Register {name} is was not included due to values set in PATH_EXCLUDES");
                    continue;
                }
                let value_reset_str = find_text_in_node_by_tag_name(&register, "resetValue")?;
                let value_reset = parse_nonneg_int_u64(value_reset_str)?;
                let address_offset_register_str =
                    find_text_in_node_by_tag_name(&register, "addressOffset")?;
                let address_offset_register = parse_nonneg_int_u64(address_offset_register_str)?;
                let access = Access::from_svd_access_type(maybe_find_text_in_node_by_tag_name(&register, "access").unwrap_or_else(|| {
                    warn!("Register {name} does not have access type. Access type is assumed to be 'read-write'.");
                    "read-write"
                }))?;
                let size_str = find_text_in_node_by_tag_name(&register, "size")?;
                let size: u64 = size_str.parse().unwrap_or_else(|_error| {
                    panic!("Failed to parse {size_str} as register size.")
                });

                let full_address = base_address + address_offset_cluster + address_offset_register;
                if let Entry::Vacant(entry) = addresses.entry(full_address) {
                    let register = Register {
                        name_peripheral: peripheral_name.clone(),
                        name_cluster: name_cluster.clone(),
                        name_register,
                        address_base: base_address,
                        address_offset_cluster,
                        address_offset_register,
                        value_reset,
                        can_read: access.is_read(),
                        can_write: access.is_write(),
                        size,
                    };
                    entry.insert(name.to_owned());
                    registers.push(register);
                } else {
                    let address_holder = addresses
                        .get(&full_address)
                        .expect("Failed to find register name by key.");
                    warn!("Register {name}'s full address is already taken by register {address_holder}. This register is ignored.");
                }
            }
        }
    }
    info!("Found {} peripherals:", peripherals.len());
    for peripheral in peripherals {
        info!("    {peripheral}");
    }
    Ok(registers)
}

/// Write found registers to output file.
fn write_output(registers: &[Register], file: &mut File) {
    let registers_as_hashmaps = registers.iter().map(Register::to_hashmap).collect_vec();
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
