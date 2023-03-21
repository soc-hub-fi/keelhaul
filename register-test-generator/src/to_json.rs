use std::collections::HashMap;

use crate::BareRegister;
use crate::RegisterUnderCluster;

/*
use crate::Register;

impl Register {
    /// Transform register structure to hashmap.
    pub fn to_hashmap(&self) -> HashMap<&str, String> {
        let mut map = HashMap::from([
            ("name_peripheral", self.peripheral_name.clone()),
            ("name_register", self.reg_name.clone()),
            ("address_base", self.base_addr.to_string()),
            ("address_offset_register", self.reg_addr_offset.to_string()),
            ("can_read", self.is_read.to_string()),
            ("can_write", self.is_write.to_string()),
            ("size", self.size.to_rust_type_str().to_string()),
        ]);
        if let Some(ref name) = self.cluster_name {
            map.insert("name_cluster", name.clone());
        }
        if let Some(offset) = self.cluster_addr_offset {
            map.insert("address_offset_cluster", offset.to_string());
        }
        if let Some(value) = self.reset_val {
            map.insert("value_reset", value.to_string());
        }
        map
    }
}
*/
