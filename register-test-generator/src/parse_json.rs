use json::JsonValue;

use crate::{JsonParseError, PtrWidth, RegPath, Register, Registers};

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
        let size = PtrWidth::try_from_rust_type_str(&get_field(value, "size")?)?;
        Ok(Register {
            // ???: I noticed that the parser assumes cluster to always exist even though it's
            // optional in the data model. Wrap in Some for now and expect breakage somewhere prior
            // to this line.
            path: RegPath::from_components(name_peripheral, Some(name_cluster), name_register),
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
