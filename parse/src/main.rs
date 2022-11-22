use common::Register;
use itertools::Itertools;
use json;
use roxmltree::{Document, Node};
use std::collections::HashMap;
use std::env;
use std::fs;
use std::fs::read_to_string;
use std::io::Write;
use std::ops::Not;
use std::path::PathBuf;

fn get_path_to_svd() -> PathBuf {
    let path_svd = env::var("PATH_SVD").expect("Missing environment variable: PATH_SVD");
    let path_svd = match PathBuf::from(path_svd).canonicalize() {
        Ok(path_svd) => path_svd,
        Err(error) => panic!("Path to SVD does not exist. {}", error,),
    };
    match path_svd.try_exists() {
        Ok(exists) => {
            if exists.not() {
                panic!("Path to SVD does not exist: {}", path_svd.display())
            }
        }
        Err(error) => panic!("{}", error),
    }
    path_svd
}

fn get_path_to_output() -> String {
    env::var("PATH_OUTPUT").expect("Missing environment variable: PATH_OUTPUT")
}

fn get_node_text_with_name(node: &Node, name: &str) -> String {
    node.children()
        .filter(|n| n.has_tag_name(name))
        .next()
        .expect("Node does not have base address.")
        .text()
        .expect("Node does not have text.")
        .to_string()
}

fn maybe_get_node_text_with_name(node: &Node, name: &str) -> Option<String> {
    match node.children().filter(|n| n.has_tag_name(name)).next() {
        Some(n) => Some(n.text().expect("Node does not have text.").to_string()),
        None => None,
    }
}

fn hex_to_int(hex: &str) -> u64 {
    let hex = hex.trim_start_matches("0x");
    u64::from_str_radix(hex, 16).expect("Failed to transform string to integer.")
}

fn main() {
    let path_svd = get_path_to_svd();
    let path_output = get_path_to_output();
    let mut file_output = fs::OpenOptions::new()
        .create(true)
        .write(true)
        .truncate(true)
        .open(path_output)
        .expect("Failed to open output file.");

    let content = read_to_string(path_svd).expect("Failed to read SVD content.");
    let parsed = Document::parse(&content).expect("Failed to parse SVD content.");

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
                let mut name = get_node_text_with_name(&register, "name");
                if name.contains("(") {
                    eprintln!("Register {}'s name contains illegal character: \"(\". This character is removed.", name);
                    name = name.replace("(", "");
                }
                if name.contains(")") {
                    eprintln!("Register {}'s name contains illegal character: \")\". This character is removed.", name);
                    name = name.replace(")", "");
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
                    eprintln!("Register {}'s full address is already taken by register {}. This register is ignored.", name, addresses.get(&full_address).unwrap());
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
    println!("Found {} registers.", registers.len());
    let registers_as_hashmaps = registers.iter().map(|r| r.to_hashmap()).collect_vec();
    let output = json::stringify(registers_as_hashmaps);
    let _ = file_output.write(output.as_bytes()).unwrap();
}
