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

/// Checks if path to a file exists, and create it if it does not exist.
///
/// # Panics
///
/// This function panics if path can not be accessed.
/// This can happen if the operating system denies access to the path.
#[inline]
#[must_use]
pub fn get_or_create(path_str: &str) -> PathBuf {
    match PathBuf::from(path_str).canonicalize() {
        Ok(path) => match path.try_exists() {
            Ok(exists) => {
                assert!(exists, "Path {} does not exist.", path.display());
                path
            }
            Err(error) => panic!("Path {} does not exist. {}", path.display(), error),
        },
        Err(error_path) => {
            println!("cargo:warning=Path {path_str} does not exist. {error_path}");
            match File::create(path_str) {
                Ok(_file) => {
                    println!("cargo:warning=Created new file to path {path_str}.");
                    validate_path_existence(path_str)
                }
                Err(error_create) => {
                    panic!("Failed to create new file to path {path_str}. {error_create}")
                }
            }
        }
    }
}

/// Represents a single memory-mapped I/O register.
#[allow(clippy::exhaustive_structs)]
pub struct Register {
    /// Name of the peripheral that register is part of.
    pub name_peripheral: String,

    /// Name of the cluster that register is part of.
    pub name_cluster: String,

    /// Name of the register.
    pub name_register: String,

    /// Address of the peripheral that register is part of.
    pub address_base: u64,

    /// Address offset of the cluster that register is part of inside peripheral address space.
    pub address_offset_cluster: u64,

    /// Address offset of the register inside cluster address space.
    pub address_offset_register: u64,

    /// Register's reset value.
    pub value_reset: u64,

    /// Register value can be read.
    pub can_read: bool,

    /// Register value can be written.
    pub can_write: bool,

    /// Bit width of the register.
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
