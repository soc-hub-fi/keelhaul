use std::collections::HashMap;

use crate::{AddrRepr, Register};

impl<T: num::CheckedAdd + ToString> Register<T> {
    /// Transform register structure to hashmap.
    pub fn to_hashmap(&self) -> HashMap<&str, String> {
        if let AddrRepr::Comps {
            base: base_addr,
            cluster: cluster_offset,
            offset: reg_offset,
        } = &self.addr
        {
            HashMap::from([
                ("name_peripheral", self.path.periph.clone()),
                // ???: cluster is assumed to always exist. This assumption is
                // incorrect and will break.
                ("name_cluster", self.path.cluster.as_ref().unwrap().clone()),
                ("name_register", self.path.reg.clone()),
                ("address_base", base_addr.to_string()),
                // ???: cluster is assumed to always exist. This assumption is
                // incorrect and will break.
                (
                    "address_offset_cluster",
                    cluster_offset.as_ref().unwrap().to_string(),
                ),
                ("address_offset_register", reg_offset.to_string()),
                // ???: reset_val is assumed to always exist. This assumption is
                // incorrect and will break.
                ("value_reset", self.reset_val.as_ref().unwrap().to_string()),
                ("access", self.access.to_string()),
                ("size", self.size.to_rust_type_str().to_string()),
            ])
        } else {
            panic!("internal error: AddrRepr must not be AddrRepr::Full in this context")
        }
    }
}
