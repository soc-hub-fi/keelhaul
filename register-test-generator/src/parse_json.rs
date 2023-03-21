use json::JsonValue;

use crate::{AddrRepr, JsonParseError, PtrWidth, RegPath, Register, Registers};

impl TryFrom<&json::object::Object> for Register<u32> {
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
        let address_offset_cluster = get_field(value, "address_offset_cluster")
            .ok()
            .map(|text| text.parse::<u32>())
            .transpose()?;
        let address_offset_register = get_field(value, "address_offset_register")?.parse()?;
        let value_reset = get_field(value, "value_reset")
            .ok()
            .map(|text| text.parse::<u64>())
            .transpose()?;
        let access = get_field(value, "access")?.parse()?;
        let size = PtrWidth::try_from_rust_type_str(&get_field(value, "size")?)?;
        Ok(Register {
            // ???: I noticed that the parser assumes cluster to always exist even though it's
            // optional in the data model. Wrap in Some for now and expect breakage somewhere prior
            // to this line.
            path: RegPath::from_components(name_peripheral, Some(name_cluster), name_register),
            addr: AddrRepr::Comps {
                base: address_base,
                cluster: address_offset_cluster,
                offset: address_offset_register,
            },
            reset_val: value_reset,
            access,
            size,
        })
    }
}

impl TryFrom<JsonValue> for Registers<u32> {
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
                    .collect::<Result<Vec<Register<u32>>, JsonParseError>>()?,
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
