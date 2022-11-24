//! SVD-file parser for register test generator.

use common::{
    force_path_existence, get_environment_variable, maybe_get_environment_variable,
    validate_path_existence, Register,
};
use itertools::Itertools;
use roxmltree::{Document, Node};
use std::{
    collections::HashMap,
    fs::{self, read_to_string, File},
    io::Write,
    path::PathBuf,
};

/// Try to extract path to excludes-file from environment variable.
fn maybe_get_path_to_excludes() -> Option<PathBuf> {
    if let Some(path_str) = maybe_get_environment_variable("PATH_EXCLUDES") {
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
                .map(|n| remove_illegal_characters(&n.to_owned()))
                .collect_vec();
            Some(registers)
        }
        None => None,
    }
}

/// Extract path to SVD-file from environment variable.
fn get_path_to_svd() -> PathBuf {
    let path_str = get_environment_variable("PATH_SVD");
    validate_path_existence(&path_str)
}

/// Read SVD-file's content.
fn get_svd_content() -> String {
    let path_svd = get_path_to_svd();
    read_to_string(path_svd).expect("Failed to read SVD content.")
}

/// Extract path to output file from environment variable.
fn get_path_to_output() -> PathBuf {
    let path_str = get_environment_variable("PATH_OUTPUT");
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
    let hex = hex.trim_start_matches("0x").trim_start_matches("0X");
    u64::from_str_radix(hex, 16).expect("Failed to transform string to integer.")
}

/// Remove illegal characters from register name.
fn remove_illegal_characters(name: &str) -> String {
    let mut name_new = name.to_owned();
    let illegals = ['(', ')'];
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
        eprintln!(
            "Register {}'s name contains {} illegal characters: {}. These characters are removed.",
            name,
            found_illegals.len(),
            symbols
        );
    }
    name_new
}

/// Find registers from XML-document.
fn find_registers(parsed: &Document, excludes: &Option<Vec<String>>) -> Vec<Register> {
    let mut registers: Vec<Register> = Vec::new();
    let mut addresses: HashMap<u64, String> = HashMap::new();
    for peripheral in parsed
        .descendants()
        .filter(|n| n.has_tag_name("peripheral"))
    {
        let address_base = get_node_text_with_name(&peripheral, "baseAddress");
        let address_base = hex_to_int(&address_base);
        for cluster in peripheral
            .descendants()
            .filter(|n| n.has_tag_name("cluster"))
        {
            let address_offset_cluster = get_node_text_with_name(&cluster, "addressOffset");
            let address_offset_cluster = hex_to_int(&address_offset_cluster);
            for register in cluster.descendants().filter(|n| n.has_tag_name("register")) {
                let name = get_node_text_with_name(&register, "name");
                let name = remove_illegal_characters(&name);
                if let Some(excluded_names) = &excludes {
                    if excluded_names.contains(&name) {
                        println!("Register {} is excluded.", name);
                        continue;
                    }
                }
                let value_reset = get_node_text_with_name(&register, "resetValue");
                let value_reset = hex_to_int(&value_reset);
                let address_offset_register = get_node_text_with_name(&register, "addressOffset");
                let address_offset_register = hex_to_int(&address_offset_register);
                let access = match maybe_get_node_text_with_name(&register, "access") {
                    Some(access) => access,
                    None => {
                        eprintln!("Register {} does not have access value. Access is assumed to be 'read-write'.", name);
                        "read-write".to_string()
                    }
                };
                let (can_read, can_write) = match access.as_str() {
                    "read-write" | "read-writeOnce" => (true, true),
                    "read-only" => (true, false),
                    "write-only" => (false, true),
                    _ => panic!("Invalid register access value: {}", access),
                };
                let full_address = address_base + address_offset_cluster + address_offset_register;
                if addresses.contains_key(&full_address) {
                    eprintln!("Register {}'s full address is already taken by register {}. This register is ignored.", name, addresses.get(&full_address).expect("Failed to find register name by key."));
                } else {
                    addresses.insert(full_address, name.clone());
                    registers.push(Register {
                        name,
                        address_base,
                        address_offset_cluster,
                        address_offset_register,
                        value_reset,
                        can_read,
                        can_write,
                    })
                }
            }
        }
    }
    registers
}

/// Write found registers to output file.
fn write_output(registers: &[Register], file: &mut File) {
    let registers_as_hashmaps = registers
        .iter()
        .map(common::Register::to_hashmap)
        .collect_vec();
    let output = json::stringify(registers_as_hashmaps);
    file.write_all(output.as_bytes())
        .expect("Failed to write to output file.");
}

pub fn parse() {
    let mut file_output = get_output_file();
    let excludes = maybe_get_excludes();
    let content = get_svd_content();
    let parsed = Document::parse(&content).expect("Failed to parse SVD content.");
    let registers = find_registers(&parsed, &excludes);
    println!("Found {} registers.", registers.len());
    write_output(&registers, &mut file_output);
}
