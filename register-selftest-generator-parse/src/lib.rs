//! SVD-file parser for register test generator.

use itertools::Itertools;
use register_selftest_generator_common::{force_path_existence, validate_path_existence, Register};
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
fn maybe_get_path_to_excludes() -> Option<PathBuf> {
    if let Ok(path_str) = env::var("PATH_EXCLUDES") {
        let path = validate_path_existence(&path_str);
        Some(path)
    } else {
        None
    }
}

/// Try to get names of excluded registers.
fn maybe_get_excludes() -> Option<Vec<String>> {
    let path_excludes = maybe_get_path_to_excludes();
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

/// Try to get names of included peripherals.
fn maybe_get_included_peripherals() -> Option<Vec<String>> {
    if let Ok(included_str) = env::var("INCLUDE_PERIPHERALS") {
        let peripherals = included_str.split(',').map(ToOwned::to_owned).collect_vec();
        // TODO: validate peripherals
        Some(peripherals)
    } else {
        None
    }
}

/// Try to get names of excluded peripherals.
fn maybe_get_excluded_peripherals() -> Option<Vec<String>> {
    if let Ok(excluded_str) = env::var("EXCLUDE_PERIPHERALS") {
        let peripherals = excluded_str.split(',').map(ToOwned::to_owned).collect_vec();
        // TODO: validate peripherals
        Some(peripherals)
    } else {
        None
    }
}

/// Extract path to SVD-file from environment variable.
fn get_path_to_svd() -> PathBuf {
    let path_svd_str = env::var("PATH_SVD").unwrap_or_else(|_| panic!("PATH_SVD must be set"));
    validate_path_existence(&path_svd_str)
}

/// Read SVD-file's content.
fn get_svd_content() -> String {
    let path_svd = get_path_to_svd();
    read_to_string(path_svd).expect("Failed to read SVD content.")
}

/// Extract path to output file from environment variable.
fn get_path_to_output() -> PathBuf {
    // Safety: OUT_DIR always exists
    let out_dir = env::var("OUT_DIR").unwrap();
    let path_str = format!("{out_dir}/parsed.json");
    force_path_existence(&path_str)
}

/// Get handle to output file.
fn get_output_file() -> File {
    let path = get_path_to_output();
    fs::OpenOptions::new()
        .create(true)
        .write(true)
        .truncate(true)
        .open(path)
        .expect("Failed to open output file.")
}

/// Find a child node with given tag name.
fn get_node_text_with_name(node: &Node, name: &str) -> String {
    node.children()
        .find(|n| n.has_tag_name(name))
        .expect("Node does not have base address.")
        .text()
        .expect("Node does not have text.")
        .to_owned()
}

/// Try to find a child node with given name.
fn maybe_get_node_text_with_name(node: &Node, name: &str) -> Option<String> {
    node.children()
        .find(|n| n.has_tag_name(name))
        .map(|n| n.text().expect("Node does not have text.").to_owned())
}

/// Transform a hexadecimal value to integer.
fn hex_to_int(hex: &str) -> u64 {
    let hex_trimmed = hex.trim_start_matches("0x").trim_start_matches("0X");
    u64::from_str_radix(hex_trimmed, 16).expect("Failed to transform string to integer.")
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

/// Find registers from SVD XML-document.
fn find_registers(
    parsed: &Document,
    excludes: &Option<Vec<String>>,
    maybe_included_peripherals: &Option<Vec<String>>,
    maybe_excluded_peripherals: &Option<Vec<String>>,
) -> Vec<Register> {
    let mut peripherals: Vec<String> = Vec::new();
    let mut registers: Vec<Register> = Vec::new();
    let mut addresses: HashMap<u64, String> = HashMap::new();
    for peripheral in parsed
        .descendants()
        .filter(|n| n.has_tag_name("peripheral"))
    {
        let address_base_str = get_node_text_with_name(&peripheral, "baseAddress");
        let address_base = hex_to_int(&address_base_str);
        let name_peripheral = get_node_text_with_name(&peripheral, "name");
        peripherals.push(name_peripheral.clone());

        if let Some(included_peripherals) = maybe_included_peripherals {
            let name_peripheral_lowercase = name_peripheral.to_lowercase();
            if !included_peripherals.contains(&name_peripheral_lowercase) {
                println!("cargo:warning=Peripheral {name_peripheral} was not included.");
                continue;
            }
        }

        if let Some(excluded_peripherals) = maybe_excluded_peripherals {
            let name_peripheral_lowercase = name_peripheral.to_lowercase();
            if excluded_peripherals.contains(&name_peripheral_lowercase) {
                println!("cargo:warning=Peripheral {name_peripheral} was excluded.");
                continue;
            }
        }

        for cluster in peripheral
            .descendants()
            .filter(|n| n.has_tag_name("cluster"))
        {
            let address_offset_cluster_str = get_node_text_with_name(&cluster, "addressOffset");
            let address_offset_cluster = hex_to_int(&address_offset_cluster_str);
            let name_cluster = get_node_text_with_name(&cluster, "name");
            for register in cluster.descendants().filter(|n| n.has_tag_name("register")) {
                let name = get_node_text_with_name(&register, "name");
                let name_register = remove_illegal_characters(&name);
                if let Some(excluded_names) = &excludes {
                    if excluded_names.contains(&name) {
                        println!("cargo:warning=Register {name} is excluded.");
                        continue;
                    }
                }
                let value_reset_str = get_node_text_with_name(&register, "resetValue");
                let value_reset = hex_to_int(&value_reset_str);
                let address_offset_register_str =
                    get_node_text_with_name(&register, "addressOffset");
                let address_offset_register = hex_to_int(&address_offset_register_str);
                let access = if let Some(access) =
                    maybe_get_node_text_with_name(&register, "access")
                {
                    access
                } else {
                    println!("cargo:warning=Register {name} does not have access value. Access is assumed to be 'read-write'.");
                    "read-write".to_owned()
                };
                let (can_read, can_write) = match access.as_str() {
                    "read-write" | "read-writeOnce" => (true, true),
                    "read-only" => (true, false),
                    "write-only" => (false, true),
                    _ => panic!("Invalid register access value: {access}"),
                };
                let size_str = get_node_text_with_name(&register, "size");
                let size: u64 = size_str.parse().unwrap_or_else(|_error| {
                    panic!("Failed to parse {size_str} as register size.")
                });

                let full_address = address_base + address_offset_cluster + address_offset_register;
                if let Entry::Vacant(entry) = addresses.entry(full_address) {
                    let register = Register {
                        name_peripheral: name_peripheral.clone(),
                        name_cluster: name_cluster.clone(),
                        name_register,
                        address_base,
                        address_offset_cluster,
                        address_offset_register,
                        value_reset,
                        can_read,
                        can_write,
                        size,
                    };
                    entry.insert(name.clone());
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
    registers
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
    let included_peripherals = maybe_get_included_peripherals();
    let excluded_peripherals = maybe_get_excluded_peripherals();
    let mut file_output = get_output_file();
    let excludes = maybe_get_excludes();
    let content = get_svd_content();
    let parsed = Document::parse(&content).expect("Failed to parse SVD content.");
    let registers = find_registers(
        &parsed,
        &excludes,
        &included_peripherals,
        &excluded_peripherals,
    );
    println!("cargo:warning=Found {} registers.", registers.len());
    write_output(&registers, &mut file_output);
}
