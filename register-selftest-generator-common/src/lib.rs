//! Common types and functions for register test generator.

// TODO: leave error handling to customer crate

use json::JsonValue;
use log::warn;
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

/// Software access rights e.g., read-only or read-write, as defined by
/// CMSIS-SVD `accessType`.
pub enum Access {
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

    pub fn is_read(&self) -> bool {
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

    pub fn is_write(&self) -> bool {
        match self {
            Access::ReadOnly => false,
            Access::WriteOnly | Access::ReadWrite | Access::WriteOnce | Access::ReadWriteOnce => {
                true
            }
        }
    }
}

#[derive(Error, Debug)]
pub enum JsonParseError {
    #[error("expected JSON object: {0}")]
    ExpectedObject(String),
    #[error("expected JSON array: {0}")]
    ExpectedArray(String),
    #[error("JSON object does not contain field for '{0}'")]
    FieldNotFound(String),
    #[error("could not parse int")]
    ParseInt(#[from] ParseIntError),
    #[error("could not parse bool")]
    ParseBool(#[from] ParseBoolError),
}

#[derive(Error, Debug)]
pub enum ParseError {
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
}

impl TryFrom<&json::object::Object> for Register {
    type Error = JsonParseError;

    fn try_from(value: &json::object::Object) -> Result<Self, Self::Error> {
        let get_field =
            |obj: &json::object::Object, field: &str| -> Result<String, JsonParseError> {
                obj.get(field)
                    .ok_or(JsonParseError::FieldNotFound(field.to_owned()))
                    .map(|x| x.to_string())
            };
        let name_peripheral = get_field(value, "name_peripheral")?;
        let name_cluster = get_field(value, "name_cluster")?;
        let name_register = get_field(value, "name_register")?;
        let address_base = get_field(value, "address_base")?.parse()?;
        let address_offset_cluster = get_field(value, "address_offset_cluster")?.parse()?;
        let address_offset_register = get_field(value, "address_offset_register")?.parse()?;
        let value_reset = get_field(value, "value_reset")?.parse()?;
        let can_read = get_field(value, "can_read")?.parse()?;
        let can_write = get_field(value, "can_write")?.parse()?;
        let size = get_field(value, "size")?.parse()?;
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
    type Error = JsonParseError;

    /// Extract registers from JSON object
    fn try_from(value: JsonValue) -> Result<Self, Self::Error> {
        match value {
            JsonValue::Array(array) => Ok(Registers::from(
                array
                    .iter()
                    .map(|value| match value {
                        JsonValue::Object(object) => Register::try_from(object),
                        _ => Err(JsonParseError::ExpectedObject(format!("{value:?}"))),
                    })
                    .collect::<Result<Vec<Register>, JsonParseError>>()?,
            )),
            _ => Err(JsonParseError::ExpectedArray(format!("{value:?}"))),
        }
    }
}

impl ops::Deref for Registers {
    type Target = Vec<Register>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
