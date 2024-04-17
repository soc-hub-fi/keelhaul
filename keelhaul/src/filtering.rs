use regex::Regex;

pub struct Filters {
    /// Filter based on register name (leaf node in input)
    pub(crate) reg: Option<ItemFilter<String>>,
    /// Filter top-level items (peripherals or subsystems)
    pub(crate) top: Option<ItemFilter<String>>,
    /// Filter the full path
    pub(crate) path_regex: Option<ItemFilter<String>>,
}

impl Filters {
    /// Take all registers in input
    pub fn all() -> Self {
        Self {
            reg: None,
            top: None,
            path_regex: None,
        }
    }

    pub fn from_filters(
        reg_filter: Option<ItemFilter<String>>,
        periph_filter: Option<ItemFilter<String>>,
        syms_filter: Option<ItemFilter<String>>,
    ) -> Self {
        Self {
            reg: reg_filter,
            top: periph_filter,
            path_regex: syms_filter,
        }
    }
}

/// What items of type `T` are allowed or not
pub enum ItemFilter<T: PartialEq> {
    List {
        // If set, only the specified items are allowed. If not set, all items are
        // allowed except the ones listed in blocklist.
        allow_list: Option<Vec<T>>,
        // These items are always blocked even if present in `white_list`
        block_list: Vec<T>,
    },
    Regex {
        // If set, only items matching the regex are allowed
        allow: Option<Regex>,
        // If set, items matching the regex are not allowed
        block: Option<Regex>,
    },
}

pub(crate) trait IsAllowedOrBlocked<V> {
    fn is_allowed(&self, value: V) -> bool;
    fn is_blocked(&self, value: V) -> bool;
}

impl<T, V> IsAllowedOrBlocked<V> for ItemFilter<T>
where
    T: PartialEq + From<V>,
    V: Into<T> + ToString + Clone,
{
    fn is_allowed(&self, value: V) -> bool {
        match self {
            Self::List {
                allow_list,
                block_list,
            } => {
                // Items in block list are always blocked
                if block_list.contains(&value.clone().into()) {
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

    fn is_blocked(&self, value: V) -> bool {
        !self.is_allowed(value)
    }
}

impl<T: PartialEq> ItemFilter<T> {
    pub fn list(white_list: Option<Vec<T>>, block_list: Vec<T>) -> ItemFilter<T> {
        Self::List {
            allow_list: white_list,
            block_list,
        }
    }

    pub const fn regex(allow: Option<Regex>, block: Option<Regex>) -> ItemFilter<T> {
        Self::Regex { allow, block }
    }
}
