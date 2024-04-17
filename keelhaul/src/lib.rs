//! Common types and functions for register test generator.

// Export full API at crate root
pub use api::*;

mod analysis;
mod api;
mod codegen;
mod error;
mod filtering;
mod frontend;
mod model;
mod util;
