//! Common types and functions for register test generator.

// TODO: leave error handling to customer crate

use json::JsonValue;
use std::{
    collections::HashMap, fs::File, num::ParseIntError, ops, path::PathBuf, str::ParseBoolError,
};
use thiserror::Error;
/// Check that path to a file exists.
///
/// # Panics
///
/// This function panics if the path does not exist.
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
pub fn get_or_create(path_str: &str) -> PathBuf {
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

#[derive(Error, Debug)]
pub enum RegisterParseError {
    #[error("expected JSON object: {0}")]
    ExpectedJsonObject(String),
    #[error("expected JSON array: {0}")]
    ExpectedJsonArray(String),
    #[error("JSON object does not contain field for '{0}'")]
    FieldNotFound(String),
    #[error("could not parse int")]
    ParseInt(#[from] ParseIntError),
    #[error("could not parse bool")]
    ParseBool(#[from] ParseBoolError),
}

/// Represents a single memory-mapped I/O register.
pub struct Register {
    pub peripheral_name: String,
    pub cluster_name: String,
    pub reg_name: String,
    pub base_addr: u64,
    pub cluster_addr_offset: u64,
    pub reg_addr_offset: u64,
    pub reset_val: u64,
    pub is_read: bool,
    pub is_write: bool,
    pub size: u64,
}

impl Register {
    /// Transform register structure to hashmap.
    pub fn to_hashmap(&self) -> HashMap<&str, String> {
        HashMap::from([
            ("name_peripheral", self.peripheral_name.clone()),
            ("name_cluster", self.cluster_name.clone()),
            ("name_register", self.reg_name.clone()),
            ("address_base", self.base_addr.to_string()),
            (
                "address_offset_cluster",
                self.cluster_addr_offset.to_string(),
            ),
            ("address_offset_register", self.reg_addr_offset.to_string()),
            ("value_reset", self.reset_val.to_string()),
            ("can_read", self.is_read.to_string()),
            ("can_write", self.is_write.to_string()),
            ("size", self.size.to_string()),
        ])
    }

    /// Get register's absolute memory address.
    pub const fn full_address(&self) -> u64 {
        self.base_addr + self.cluster_addr_offset + self.reg_addr_offset
    }

    /// Get register's unique identifier.
    pub fn uid(&self) -> String {
        format!(
            "{}-{}-{}",
            self.peripheral_name, self.cluster_name, self.reg_name
        )
    }

    pub fn json_object_to_register(
        object: &json::object::Object,
    ) -> Result<Register, RegisterParseError> {
        let get_field =
            |obj: &json::object::Object, field: &str| -> Result<String, RegisterParseError> {
                obj.get(field)
                    .ok_or(RegisterParseError::FieldNotFound(field.to_owned()))
                    .map(|x| x.to_string())
            };
        let name_peripheral = get_field(object, "name_peripheral")?;
        let name_cluster = get_field(object, "name_cluster")?;
        let name_register = get_field(object, "name_register")?;
        let address_base = get_field(object, "address_base")?.parse()?;
        let address_offset_cluster = get_field(object, "address_offset_cluster")?.parse()?;
        let address_offset_register = get_field(object, "address_offset_register")?.parse()?;
        let value_reset = get_field(object, "value_reset")?.parse()?;
        let can_read = get_field(object, "can_read")?.parse()?;
        let can_write = get_field(object, "can_write")?.parse()?;
        let size = get_field(object, "size")?.parse()?;
        Ok(Register {
            peripheral_name: name_peripheral,
            cluster_name: name_cluster,
            reg_name: name_register,
            base_addr: address_base,
            cluster_addr_offset: address_offset_cluster,
            reg_addr_offset: address_offset_register,
            reset_val: value_reset,
            is_read: can_read,
            is_write: can_write,
            size,
        })
    }
}

/// A list of registers parsed from SVD or IP-XACT (newtype).
pub struct Registers(Vec<Register>);

impl From<Vec<Register>> for Registers {
    fn from(value: Vec<Register>) -> Self {
        Self(value)
    }
}

impl TryFrom<JsonValue> for Registers {
    type Error = RegisterParseError;

    /// Extract registers from JSON object
    fn try_from(value: JsonValue) -> Result<Self, Self::Error> {
        match value {
            JsonValue::Array(array) => Ok(Registers::from(
                array
                    .iter()
                    .map(|value| match value {
                        JsonValue::Object(object) => Register::json_object_to_register(object),
                        _ => Err(RegisterParseError::ExpectedJsonObject(format!("{value:?}"))),
                    })
                    .collect::<Result<Vec<Register>, RegisterParseError>>()?,
            )),
            _ => Err(RegisterParseError::ExpectedJsonArray(format!("{value:?}"))),
        }
    }
}

impl ops::Deref for Registers {
    type Target = Vec<Register>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
