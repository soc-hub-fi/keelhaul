use std::collections::HashMap;

use crate::Register;

impl Register {
    /// Transform register structure to hashmap.
    pub fn to_hashmap(&self) -> HashMap<&str, String> {
        HashMap::from([
            ("name_peripheral", self.path.periph.clone()),
            // ???: cluster is assumed to always exist. This assumption is
            // incorrect and will break.
            ("name_cluster", self.path.cluster.as_ref().unwrap().clone()),
            ("name_register", self.path.reg.clone()),
            ("address_base", self.base_addr.to_string()),
            (
                "address_offset_cluster",
                self.cluster_addr_offset.to_string(),
            ),
            ("address_offset_register", self.reg_addr_offset.to_string()),
            ("value_reset", self.reset_val.to_string()),
            ("can_read", self.is_read.to_string()),
            ("can_write", self.is_write.to_string()),
            ("size", self.size.to_rust_type_str().to_string()),
        ])
    }
}
