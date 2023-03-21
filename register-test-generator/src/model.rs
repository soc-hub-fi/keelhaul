//! Encodes information about memory mapped registers. This information can be
//! used to generate test cases.
use log::warn;
use std::{collections::HashMap, ops};

use crate::{AddrRepr, GenerateError, ParseError};

/// Software access rights e.g., read-only or read-write, as defined by
/// CMSIS-SVD `accessType`.
#[derive(Clone)]
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
    /// Whether this register is software readable or not
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

    /// Whether this register is software writable or not
    pub fn is_write(&self) -> bool {
        match self {
            Access::ReadOnly => false,
            Access::WriteOnly | Access::ReadWrite | Access::WriteOnce | Access::ReadWriteOnce => {
                true
            }
        }
    }

    pub fn to_string(&self) -> String {
        match self {
            Self::ReadOnly => "ReadOnly",
            Self::WriteOnly => "WriteOnly",
            Self::ReadWrite => "ReadWrite",
            Self::WriteOnce => "WriteOnce",
            Self::ReadWriteOnce => "ReadWriteOnce",
        }
        .to_owned()
    }

    pub fn from_string(str: &str) -> Self {
        match str {
            "ReadOnly" => Self::ReadOnly,
            "WriteOnly" => Self::WriteOnly,
            "ReadWrite" => Self::ReadWrite,
            "WriteOnce" => Self::WriteOnce,
            "ReadWriteOnce" => Self::ReadWriteOnce,
            _ => panic!("Invalid register access: {}", str),
        }
    }
}

#[derive(Clone)]
pub enum PtrWidth {
    U8,
    U16,
    U32,
    U64,
}

impl PtrWidth {
    /// E.g., u8, u16, u32, u64
    pub fn to_rust_type_str(&self) -> &str {
        match self {
            PtrWidth::U8 => "u8",
            PtrWidth::U16 => "u16",
            PtrWidth::U32 => "u32",
            PtrWidth::U64 => "u64",
        }
    }

    pub fn from_rust_type_str(str: &str) -> Self {
        match str {
            "u8" => Self::U8,
            "u16" => Self::U16,
            "u32" => Self::U32,
            "u64" => Self::U64,
            _ => panic!("Invalid pointer width: {}", str),
        }
    }

    pub fn from_bit_count(bc: u64) -> Result<Self, ParseError> {
        match bc {
            8 => Ok(PtrWidth::U8),
            16 => Ok(PtrWidth::U16),
            32 => Ok(PtrWidth::U32),
            64 => Ok(PtrWidth::U64),
            bc => Err(ParseError::BitCountToPtrWidth(bc)),
        }
    }
}

/// A list of registers parsed from SVD or IP-XACT (newtype).
pub struct Registers(pub Vec<Box<dyn IsRegister>>);

impl From<Vec<Box<dyn IsRegister>>> for Registers {
    fn from(value: Vec<Box<dyn IsRegister>>) -> Self {
        Self(value)
    }
}

impl ops::Deref for Registers {
    type Target = Vec<Box<dyn IsRegister>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Clone)]
pub struct RegisterProperties {
    pub size: Option<PtrWidth>,
    pub access: Access,
    // pub protection: Option<Protection>,
    pub reset_value: Option<u64>,
    pub reset_mask: Option<u64>,
}

impl RegisterProperties {
    fn to_hashmap(&self) -> HashMap<String, String> {
        let mut map = HashMap::new();
        if let Some(ref size) = self.size {
            map.insert(
                "register_size".to_owned(),
                size.to_rust_type_str().to_owned(),
            );
        }
        map.insert("register_access".to_owned(), self.access.to_string());
        if let Some(reset_value) = self.reset_value {
            map.insert("register_reset_value".to_owned(), reset_value.to_string());
        }
        if let Some(reset_mask) = self.reset_mask {
            map.insert("register_reset_mask".to_owned(), reset_mask.to_string());
        }
        map
    }
}

#[derive(Clone)]
pub struct ProcessedRegister {
    pub name: String,
    pub addr_offset: u64,
    pub properties: RegisterProperties,
}

#[derive(Clone)]
pub struct ProcessedCluster {
    pub name: String,
    pub addr_offset: u64,
    pub registers: Vec<ProcessedRegister>,
}

#[derive(Clone)]
pub struct ProcessedPeripheral {
    pub name: String,
    pub base_addr: u64,
    pub registers: Vec<ProcessedRegister>,
    pub clusters: Vec<ProcessedCluster>,
}

pub trait IsRegister {
    /// Get register's absolute memory address.
    fn full_address(&self) -> Result<u64, GenerateError>;
    /// Get register's taxonomy.
    fn full_path(&self, sep: &str) -> String;
    fn is_read(&self) -> bool;
    fn is_write(&self) -> bool;
    fn reset_value(&self) -> Option<u64>;
    fn to_hashmap(&self) -> HashMap<String, String>;
    fn peripheral(&self) -> &ProcessedPeripheral;
    fn register(&self) -> &ProcessedRegister;
    fn uid(&self) -> String;
    fn size(&self) -> PtrWidth;
}

pub struct BareRegister {
    pub peripheral: ProcessedPeripheral,
    pub register: ProcessedRegister,
}

impl IsRegister for BareRegister {
    fn full_address(&self) -> Result<u64, GenerateError> {
        let base = self.peripheral.base_addr;
        let offset = self.register.addr_offset;
        let err = GenerateError::AddrOverflow(
            self.full_path("-"),
            AddrRepr::Comps {
                base,
                cluster: None,
                offset,
            },
        );
        base.checked_add(offset).ok_or(err)
    }
    fn full_path(&self, sep: &str) -> String {
        vec![self.peripheral.name.clone(), self.register.name.clone()].join(sep)
    }
    fn is_read(&self) -> bool {
        // TODO
        true
    }
    fn is_write(&self) -> bool {
        // TODO
        false
    }
    fn reset_value(&self) -> Option<u64> {
        // TODO
        None
    }
    fn peripheral(&self) -> &ProcessedPeripheral {
        &self.peripheral
    }
    fn register(&self) -> &ProcessedRegister {
        &self.register
    }
    fn uid(&self) -> String {
        self.full_path("-")
    }
    fn size(&self) -> PtrWidth {
        // TODO
        PtrWidth::U64
    }
    fn to_hashmap(&self) -> HashMap<String, String> {
        let mut map = HashMap::from([
            ("register_name".to_owned(), self.register.name.clone()),
            (
                "register_offset".to_owned(),
                self.register.addr_offset.to_string(),
            ),
            ("peripheral_name".to_owned(), self.peripheral.name.clone()),
            (
                "peripheral_base_address".to_owned(),
                self.peripheral.base_addr.to_string(),
            ),
        ]);
        map.extend(self.register.properties.to_hashmap());
        map
    }
}

pub struct RegisterUnderCluster {
    pub peripheral: ProcessedPeripheral,
    pub cluster: ProcessedCluster,
    pub register: ProcessedRegister,
}

impl IsRegister for RegisterUnderCluster {
    fn full_address(&self) -> Result<u64, GenerateError> {
        let base = self.peripheral.base_addr;
        let cluster = self.cluster.addr_offset;
        let offset = self.register.addr_offset;
        let err = GenerateError::AddrOverflow(
            self.full_path("-"),
            AddrRepr::Comps {
                base,
                cluster: Some(cluster),
                offset,
            },
        );
        base.checked_add(cluster)
            .and_then(|x| x.checked_add(offset))
            .ok_or(err)
    }
    fn full_path(&self, sep: &str) -> String {
        vec![
            self.peripheral.name.clone(),
            self.cluster.name.clone(),
            self.register.name.clone(),
        ]
        .join(sep)
    }
    fn is_read(&self) -> bool {
        // TODO
        true
    }
    fn is_write(&self) -> bool {
        // TODO
        false
    }
    fn reset_value(&self) -> Option<u64> {
        // TODO
        None
    }

    fn peripheral(&self) -> &ProcessedPeripheral {
        &self.peripheral
    }
    fn register(&self) -> &ProcessedRegister {
        &self.register
    }
    fn uid(&self) -> String {
        self.full_path("-")
    }
    fn size(&self) -> PtrWidth {
        // TODO
        PtrWidth::U64
    }
    fn to_hashmap(&self) -> HashMap<String, String> {
        let mut map = HashMap::from([
            ("register_name".to_owned(), self.register.name.clone()),
            (
                "register_offset".to_owned(),
                self.register.addr_offset.to_string(),
            ),
            ("cluster_name".to_owned(), self.cluster.name.clone()),
            (
                "cluster_offset".to_owned(),
                self.cluster.addr_offset.to_string(),
            ),
            ("peripheral_name".to_owned(), self.peripheral.name.clone()),
            (
                "peripheral_base_address".to_owned(),
                self.peripheral.base_addr.to_string(),
            ),
        ]);
        map.extend(self.register.properties.to_hashmap());
        map
    }
}
