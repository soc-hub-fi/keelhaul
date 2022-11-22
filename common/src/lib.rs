use std::collections::HashMap;

pub struct Register {
    pub name: String,
    pub address_base: u64,
    pub address_offset_cluster: u64,
    pub address_offset_register: u64,
    pub value_reset: u64,
    pub can_read: bool,
    pub can_write: bool,
}

impl Register {
    pub fn to_hashmap(&self) -> HashMap<&str, String> {
        HashMap::from([
            ("name", self.name.clone()),
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
        ])
    }
}
