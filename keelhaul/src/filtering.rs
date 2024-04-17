use regex::Regex;

pub struct Filters {
    /// Filter top-level items (peripherals or subsystems)
    pub(crate) top: Option<ItemFilter>,
    /// Filter based on register name (leaf node in input)
    pub(crate) reg: Option<ItemFilter>,
    /// Filter based on the full path to the register
    ///
    /// Dash ('-') is used as the separator between path segments, e.g.,
    /// "PERIPH_NAME-CLUSTER_NAME-REG_NAME".
    pub(crate) path: Option<ItemFilter>,
}

impl Filters {
    /// Take all registers in input
    pub fn all() -> Self {
        Self {
            reg: None,
            top: None,
            path: None,
        }
    }

    /// Specify each filter
    pub fn from_filters(
        reg_filter: Option<ItemFilter>,
        periph_filter: Option<ItemFilter>,
        syms_filter: Option<ItemFilter>,
    ) -> Self {
        Self {
            reg: reg_filter,
            top: periph_filter,
            path: syms_filter,
        }
    }
}

/// Filter out items by blocking them, or by setting an allow list
pub enum ItemFilter {
    List {
        // If set, only the specified items are allowed. If not set, all items are
        // allowed except the ones listed in blocklist.
        allow_list: Option<Vec<String>>,
        // These items are always blocked even if present in `white_list`
        block_list: Vec<String>,
    },
    Regex {
        // If set, only items matching the regex are allowed
        allow: Option<Regex>,
        // If set, items matching the regex are not allowed
        block: Option<Regex>,
    },
}

pub(crate) trait IsAllowedOrBlocked {
    fn is_allowed(&self, value: &str) -> bool;
    fn is_blocked(&self, value: &str) -> bool {
        !self.is_allowed(value)
    }
}

impl IsAllowedOrBlocked for ItemFilter {
    fn is_allowed(&self, value: &str) -> bool {
        match self {
            Self::List {
                allow_list,
                block_list,
            } => {
                // Items in block list are always blocked
                if block_list.contains(&value.into()) {
                    return false;
                }
                allow_list
                    .as_ref()
                    .map_or(true, |wl| wl.contains(&value.into()))
            }
            Self::Regex { allow, block } => {
                let value_string = value.to_string();
                let value = value_string.as_ref();
                // Items matched by block regex are always blocked
                if let Some(block) = block {
                    if block.is_match(value) {
                        return false;
                    }
                }
                allow.as_ref().map_or(true, |allow| allow.is_match(value))
            }
        }
    }
}

impl ItemFilter {
    pub fn list(white_list: Option<Vec<String>>, block_list: Vec<String>) -> ItemFilter {
        Self::List {
            allow_list: white_list,
            block_list,
        }
    }

    pub const fn regex(allow: Option<Regex>, block: Option<Regex>) -> ItemFilter {
        Self::Regex { allow, block }
    }
}
