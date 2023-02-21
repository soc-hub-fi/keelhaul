//! Common types and functions for register test generator.

// TODO: leave error handling to customer crate

use std::{collections::HashMap, fs::File, path::PathBuf};

/// Check that path to a file exists.
///
/// # Panics
///
/// This function panics if the path does not exist.
#[inline]
#[must_use]
pub fn validate_path_existence(path_str: &str) -> PathBuf {
    match PathBuf::from(path_str).canonicalize() {
        Ok(path) => match path.try_exists() {
            Ok(exists) => {
                assert!(exists, "Path {} does not exist.", path.display());
                path
            }
            Err(error) => panic!("Path {} does not exist. {}", path.display(), error),
        },
        Err(error) => panic!("Path {path_str} does not exist. {error}"),
    }
}

/// Checks if path to a file exists, and creates it if it does not exist.
///
/// # Panics
///
/// This function panics if path can not be accessed.
/// This can happen if the operating system denies access to the path.
#[inline]
#[must_use]
pub fn force_path_existence(path_str: &str) -> PathBuf {
    match PathBuf::from(path_str).canonicalize() {
        Ok(path) => match path.try_exists() {
            Ok(exists) => {
                assert!(exists, "Path {} does not exist.", path.display());
                path
            }
            Err(error) => panic!("Path {} does not exist. {}", path.display(), error),
        },
        Err(error) => {
            println!("Path {path_str} does not exist. {error}");
            match File::create(path_str) {
                Ok(_file) => {
                    println!("Created new file to path {path_str}.");
                    validate_path_existence(path_str)
                }
                Err(error) => panic!("Failed to create new file to path {path_str}. {error}"),
            }
        }
    }
}

/// Represents a single memory-mapped I/O register.
pub struct Register {
    pub name_peripheral: String,
    pub name_cluster: String,
    pub name_register: String,
    pub address_base: u64,
    pub address_offset_cluster: u64,
    pub address_offset_register: u64,
    pub value_reset: u64,
    pub can_read: bool,
    pub can_write: bool,
    pub size: u64,
}

impl Register {
    /// Transform register structure to hashmap.
    #[inline]
    #[must_use]
    pub fn to_hashmap(&self) -> HashMap<&str, String> {
        HashMap::from([
            ("name_peripheral", self.name_peripheral.clone()),
            ("name_cluster", self.name_cluster.clone()),
            ("name_register", self.name_register.clone()),
            ("address_base", self.address_base.to_string()),
            (
                "address_offset_cluster",
                self.address_offset_cluster.to_string(),
            ),
            (
                "address_offset_register",
                self.address_offset_register.to_string(),
            ),
            ("value_reset", self.value_reset.to_string()),
            ("can_read", self.can_read.to_string()),
            ("can_write", self.can_write.to_string()),
            ("size", self.size.to_string()),
        ])
    }

    /// Get register's absolute memory address.
    #[inline]
    #[must_use]
    pub const fn full_address(&self) -> u64 {
        self.address_base + self.address_offset_cluster + self.address_offset_register
    }

    /// Get register's unique identifier.
    #[inline]
    #[must_use]
    pub fn uid(&self) -> String {
        format!(
            "{}-{}-{}",
            self.name_peripheral, self.name_cluster, self.name_register
        )
    }
}
