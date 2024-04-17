use std::{ops, str};

use regex::Regex;

pub struct Filters {
    /// Filter top-level items (peripherals or subsystems)
    pub(crate) top: Option<Box<dyn Filter>>,
    /// Filter based on register name (leaf node in input)
    pub(crate) reg: Option<Box<dyn Filter>>,
    /// Filter based on the full path to the register
    ///
    /// Dash ('-') is used as the separator between path segments, e.g.,
    /// "PERIPH_NAME-CLUSTER_NAME-REG_NAME".
    pub(crate) path: Option<Box<dyn Filter>>,
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
        reg: Option<Box<dyn Filter>>,
        top: Option<Box<dyn Filter>>,
        path: Option<Box<dyn Filter>>,
    ) -> Self {
        Self { reg, top, path }
    }
}

pub trait Filter {
    fn is_allowed(&self, value: &str) -> bool;
    fn is_blocked(&self, value: &str) -> bool {
        !self.is_allowed(value)
    }
}

/// Filter out items by blocking them, or by setting an allow list
pub struct ListFilter {
    // If set, only the specified items are allowed. If not set, all items are
    // allowed except the ones listed in blocklist.
    allow_list: Option<Vec<String>>,
    // These items are always blocked even if present in `white_list`
    block_list: Vec<String>,
}

impl ListFilter {
    pub fn new(allow_list: Option<Vec<String>>, block_list: Vec<String>) -> ListFilter {
        ListFilter {
            allow_list,
            block_list,
        }
    }
}

impl Filter for ListFilter {
    fn is_allowed(&self, value: &str) -> bool {
        // Items in block list are always blocked
        if self.block_list.contains(&value.into()) {
            return false;
        }
        self.allow_list
            .as_ref()
            .map_or(true, |wl| wl.contains(&value.into()))
    }
}

/// Filter items by regex
///
/// Only matching items are allowed
pub struct RegexFilter(Regex);

impl RegexFilter {
    pub const fn new(re: Regex) -> RegexFilter {
        RegexFilter(re)
    }
}

impl str::FromStr for RegexFilter {
    type Err = regex::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self::new(Regex::new(s)?))
    }
}

impl Filter for RegexFilter {
    fn is_allowed(&self, value: &str) -> bool {
        let value_string = value.to_string();
        let value = value_string.as_ref();

        self.0.is_match(value)
    }
}

impl ops::Deref for RegexFilter {
    type Target = Regex;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
