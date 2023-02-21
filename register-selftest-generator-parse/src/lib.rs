//! SVD-file parser for register test generator.

use itertools::Itertools;
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

/// Read an environment variable into a Vec<String>
///
/// # Parameters:
///
/// `var` - The name of the environment variable
/// `sep` - The separator for Vec elements
///
/// Returns None if the environment variable is not present
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

/// Extract path to output file from environment variable.
/// Get handle to output file.
fn open_output_file() -> File {
    // Safety: OUT_DIR always exists
    let out_dir = env::var("OUT_DIR").unwrap();
    let path_str = format!("{out_dir}/parsed.json");
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
fn remove_illegal_characters(name: &str) -> String {
    let mut name_new = name.to_owned();
    let illegals = ['(', ')', '[', ']', '%'];
    let mut found_illegals = Vec::new();
    for illegal in illegals {
        if name_new.contains(illegal) {
            found_illegals.push(illegal);
            name_new = name_new.replace(illegal, "");
        }
    }
    if !found_illegals.is_empty() {
        let symbols = found_illegals
            .iter()
            .map(|c| format!("\"{}\"", c.to_owned()))
            .join(", ");
        println!(
            "cargo:warning=Register {}'s name contains {} illegal characters: {}. These characters are removed.",
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

/// Find registers from SVD XML-document.
fn find_registers(
    parsed: &Document,
    excludes: &Option<Vec<String>>,
    maybe_included_peripherals: &Option<Vec<String>>,
    maybe_excluded_peripherals: &Option<Vec<String>>,
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
        let peripheral_name = find_text_in_node_by_tag_name(&peripheral_node, "name")?;
        peripherals.push(peripheral_name.to_owned());

        if let Some(included_peripherals) = maybe_included_peripherals {
            let peripheral_name_lc = peripheral_name.to_lowercase();
            if !included_peripherals.contains(&peripheral_name_lc) {
                println!("cargo:warning=Peripheral {peripheral_name} was not included.");
                continue;
            }
        }

        if let Some(excluded_peripherals) = maybe_excluded_peripherals {
            let peripheral_name_lc = peripheral_name.to_lowercase();
            if excluded_peripherals.contains(&peripheral_name_lc) {
                println!("cargo:warning=Peripheral {peripheral_name} was excluded.");
                continue;
            }
        }

        for cluster in peripheral_node
            .descendants()
            .filter(|n| n.has_tag_name("cluster"))
        {
            let address_offset_cluster_str =
                find_text_in_node_by_tag_name(&cluster, "addressOffset")?;
            let address_offset_cluster = parse_nonneg_int_u64(address_offset_cluster_str)?;
            let name_cluster = find_text_in_node_by_tag_name(&cluster, "name")?;
            for register in cluster.descendants().filter(|n| n.has_tag_name("register")) {
                let name = find_text_in_node_by_tag_name(&register, "name")?;
                let name_register = remove_illegal_characters(name);
                if let Some(excluded_names) = &excludes {
                    if excluded_names.contains(&name.to_string()) {
                        println!("cargo:warning=Register {name} is excluded.");
                        continue;
                    }
                }
                let value_reset_str = find_text_in_node_by_tag_name(&register, "resetValue")?;
                let value_reset = parse_nonneg_int_u64(value_reset_str)?;
                let address_offset_register_str =
                    find_text_in_node_by_tag_name(&register, "addressOffset")?;
                let address_offset_register = parse_nonneg_int_u64(address_offset_register_str)?;
                let access = if let Some(access) =
                    maybe_find_text_in_node_by_tag_name(&register, "access")
                {
                    access
                } else {
                    println!("cargo:warning=Register {name} does not have access value. Access is assumed to be 'read-write'.");
                    "read-write"
                };
                let (can_read, can_write) = match access {
                    "read-write" | "read-writeOnce" => (true, true),
                    "read-only" => (true, false),
                    "write-only" => (false, true),
                    _ => panic!("Invalid register access value: {access}"),
                };
                let size_str = find_text_in_node_by_tag_name(&register, "size")?;
                let size: u64 = size_str.parse().unwrap_or_else(|_error| {
                    panic!("Failed to parse {size_str} as register size.")
                });

                let full_address = base_address + address_offset_cluster + address_offset_register;
                if let Entry::Vacant(entry) = addresses.entry(full_address) {
                    let register = Register {
                        name_peripheral: peripheral_name.to_owned(),
                        name_cluster: name_cluster.to_owned(),
                        name_register,
                        address_base: base_address,
                        address_offset_cluster,
                        address_offset_register,
                        value_reset,
                        can_read,
                        can_write,
                        size,
                    };
                    entry.insert(name.to_owned());
                    registers.push(register);
                } else {
                    let address_holder = addresses
                        .get(&full_address)
                        .expect("Failed to find register name by key.");
                    println!("cargo:warning=Register {name}'s full address is already taken by register {address_holder}. This register is ignored.");
                }
            }
        }
    }
    println!("cargo:warning=Found {} peripherals:", peripherals.len());
    for peripheral in peripherals {
        println!("cargo:warning=    {peripheral}");
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
    let included_peripherals = read_vec_from_env("INCLUDE_PERIPHERALS", ',');
    let excluded_peripherals = read_vec_from_env("EXCLUDE_PERIPHERALS", ',');
    let mut file_output = open_output_file();
    let excludes = read_excludes_from_env();
    let content = read_input_svd_to_string();
    let parsed = Document::parse(&content).expect("Failed to parse SVD content.");
    let registers = find_registers(
        &parsed,
        &excludes,
        &included_peripherals,
        &excluded_peripherals,
    )
    .unwrap();
    println!("cargo:warning=Found {} registers.", registers.len());
    write_output(&registers, &mut file_output);
}
