use json::JsonValue;

use crate::{model::*, JsonParseError, PtrWidth, Registers};
use log::{info, warn};

/*
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
        let name_cluster = match get_field(value, "name_cluster") {
            Ok(name) => Some(name),
            Err(_) => None,
        };
        let name_register = get_field(value, "name_register")?;
        let address_base = get_field(value, "address_base")?.parse()?;
        let address_offset_cluster = match get_field(value, "address_offset_cluster") {
            Ok(address) => Some(address.parse()?),
            Err(_) => None,
        };
        let address_offset_register = get_field(value, "address_offset_register")?.parse()?;
        let value_reset = match get_field(value, "value_reset") {
            Ok(value) => Some(value.parse()?),
            Err(_) => None,
        };
        let can_read = get_field(value, "can_read")?.parse()?;
        let can_write = get_field(value, "can_write")?.parse()?;
        let size = PtrWidth::try_from_rust_type_str(&get_field(value, "size")?)?;
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
*/

fn get_field(obj: &json::object::Object, field: &str) -> Result<String, JsonParseError> {
    obj.get(field)
        .ok_or(JsonParseError::FieldNotFound(field.to_owned()))
        .map(|x| x.to_string())
}

fn maybe_get_field(obj: &json::object::Object, field: &str) -> Option<String> {
    match obj.get(field) {
        Some(value) => Some(value.to_string()),
        None => None,
    }
}

impl TryFrom<&json::object::Object> for ProcessedRegister {
    type Error = JsonParseError;

    fn try_from(value: &json::object::Object) -> Result<Self, Self::Error> {
        let name = get_field(value, "register_name")?;
        let addr_offset = get_field(value, "register_offset")?.parse()?;

        let size = match maybe_get_field(value, "register_size") {
            Some(value) => {
                let size = PtrWidth::from_rust_type_str(&value);
                Some(size)
            }
            None => None,
        };
        let access = Access::from_string(&get_field(value, "register_access")?);
        let reset_value = match maybe_get_field(value, "register_reset_value") {
            Some(value) => Some(value.parse()?),
            None => None,
        };
        let reset_mask = match maybe_get_field(value, "register_reset_mask") {
            Some(value) => Some(value.parse()?),
            None => None,
        };

        let properties = RegisterProperties {
            size,
            access,
            reset_value,
            reset_mask,
        };
        Ok(Self {
            name,
            addr_offset,
            properties,
        })
    }
}

impl TryFrom<&json::object::Object> for ProcessedCluster {
    type Error = JsonParseError;

    fn try_from(value: &json::object::Object) -> Result<Self, Self::Error> {
        let name = get_field(value, "cluster_name")?;
        let addr_offset = get_field(value, "cluster_offset")?.parse()?;
        // TODO
        let registers = Vec::new();
        Ok(Self {
            name,
            addr_offset,
            registers,
        })
    }
}

impl TryFrom<&json::object::Object> for ProcessedPeripheral {
    type Error = JsonParseError;

    fn try_from(value: &json::object::Object) -> Result<Self, Self::Error> {
        let name = get_field(value, "peripheral_name")?;
        let base_addr = get_field(value, "peripheral_base_address")?.parse()?;
        // TODO
        let registers = Vec::new();
        let clusters = Vec::new();
        Ok(Self {
            name,
            base_addr,
            registers,
            clusters,
        })
    }
}

fn json_object_to_register(
    value: &json::object::Object,
) -> Result<Box<dyn IsRegister>, JsonParseError> {
    let register = ProcessedRegister::try_from(value)?;
    let peripheral = ProcessedPeripheral::try_from(value)?;
    let register: Box<dyn IsRegister> = match ProcessedCluster::try_from(value) {
        Ok(cluster) => Box::new(RegisterUnderCluster {
            peripheral,
            cluster,
            register,
        }),
        Err(_) => {
            // TODO: is there a better way to detect non-existence of cluster?
            Box::new(BareRegister {
                peripheral,
                register,
            })
        }
    };
    Ok(register)
}

impl TryFrom<JsonValue> for Registers {
    type Error = JsonParseError;

    /// Extract registers from JSON object
    fn try_from(value: JsonValue) -> Result<Self, Self::Error> {
        match value {
            JsonValue::Array(array) => {
                let mut registers = Vec::new();
                for json_value in &array {
                    match json_value {
                        JsonValue::Object(object) => {
                            let register = json_object_to_register(object)?;
                            registers.push(register);
                        }
                        _ => panic!(""),
                    }
                }
                Ok(Self(registers))
            }
            // FIXME: implement TryFrom<&json::object::Object> for Box<dyn IsRegister>
            /*{
                Ok(Registers::from(
                    array
                        .iter()
                        .map(|value| match value {
                            JsonValue::Object(object) => Box<dyn IsRegister>::try_from(object),
                            _ => Err(JsonParseError::ExpectedObject(format!("{value:?}"))),
                        })
                        .collect::<Result<Vec<Box<dyn IsRegister>>, JsonParseError>>()?,
                ))
            }*/
            _ => Err(JsonParseError::ExpectedArray(format!("{value:?}"))),
        }
    }
}

impl PtrWidth {
    pub fn try_from_rust_type_str(s: &str) -> Result<PtrWidth, JsonParseError> {
        match s {
            "u8" => Ok(PtrWidth::U8),
            "u16" => Ok(PtrWidth::U16),
            "u32" => Ok(PtrWidth::U32),
            "u64" => Ok(PtrWidth::U64),
            _ => Err(JsonParseError::ParseTypeStr(s.to_owned())),
        }
    }
}
