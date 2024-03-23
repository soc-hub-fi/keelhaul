//! Common types and functions for register test generator.

// Export full API at crate root
pub use api::*;

mod api;
// TODO: leave error handling to user crate
pub mod error;
mod generate;
mod model;
mod parse_svd;

pub use generate::*;
pub use model::*;
pub use parse_svd::*;
use regex::Regex;

use fs_err as fs;
use std::path::Path;

/// Returns contents of a file at `path`, panicking on any failure
///
/// # Panics
///
/// This function panics if the path does not exist, or if the file cannot be
/// read.
#[must_use]
pub fn read_file_or_panic(path: &Path) -> String {
    path.canonicalize().map_or_else(
        |err| panic!("path {} does not exist: {err}", path.display()),
        |p| {
            fs::read_to_string(p)
                .unwrap_or_else(|err| panic!("cannot read file at path {}: {err}", path.display()))
        },
    )
}

/// What items of type `T` are allowed or not
pub enum ItemFilter<T: PartialEq> {
    List {
        // If set, only the specified items are allowed. If not set, all items are
        // allowed except the ones listed in blocklist.
        white_list: Option<Vec<T>>,
        // These items are always blocked even if present in `white_list`
        block_list: Vec<T>,
    },
    Regex {
        allow: Option<Regex>,
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
                white_list,
                block_list,
            } => {
                // Items in block list are always blocked
                if block_list.contains(&value.clone().into()) {
                    return false;
                }
                white_list
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
    #[allow(clippy::use_self)]
    pub fn list(white_list: Option<Vec<T>>, block_list: Vec<T>) -> ItemFilter<T> {
        Self::List {
            white_list,
            block_list,
        }
    }

    #[allow(clippy::use_self)]
    pub const fn regex(allow: Option<Regex>, block: Option<Regex>) -> ItemFilter<T> {
        Self::Regex { allow, block }
    }
}

// TODO: Move ItemFilter to api.rs

pub struct Filters {
    pub(crate) reg_filter: Option<ItemFilter<String>>,
    pub(crate) periph_filter: Option<ItemFilter<String>>,
    pub(crate) syms_regex: Option<ItemFilter<String>>,
}

impl Filters {
    /// Take all registers in input
    pub fn all() -> Self {
        Self {
            reg_filter: None,
            periph_filter: None,
            syms_regex: None,
        }
    }

    pub fn from_filters(
        reg_filter: ItemFilter<String>,
        periph_filter: ItemFilter<String>,
        syms_filter: ItemFilter<String>,
    ) -> Self {
        Self {
            reg_filter: Some(reg_filter),
            periph_filter: Some(periph_filter),
            syms_regex: Some(syms_filter),
        }
    }
}
