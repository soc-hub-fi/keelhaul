//! Common types and functions for register test generator.

// Export full API at crate root
pub use api::*;

pub(crate) mod analysis;
mod api;
pub(crate) mod codegen;
pub mod error;
mod filtering;
pub(crate) mod frontend;
mod model;
mod util;

pub use codegen::*;
