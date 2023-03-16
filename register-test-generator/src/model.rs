use json::JsonValue;
use log::warn;
use std::{collections::HashMap, ops};

use crate::{AddrRepr, GenerateError, JsonParseError, ParseError};

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
    pub fn full_address(&self) -> Result<u64, GenerateError> {
        let base = self.base_addr;
        let cluster = self.cluster_addr_offset;
        let offset = self.reg_addr_offset;
        let err = GenerateError::AddrOverflow(
            self.full_path("-"),
            AddrRepr::Comps {
                base,
                cluster,
                offset,
            },
        );
        base.checked_add(cluster)
            .and_then(|x| x.checked_add(offset))
            .ok_or(err)
    }

    pub fn full_path(&self, sep: &str) -> String {
        [
            self.peripheral_name.clone(),
            self.cluster_name.clone(),
            self.reg_name.clone(),
        ]
        .join(sep)
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
