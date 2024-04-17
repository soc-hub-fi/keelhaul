//! Keelhaul --- Generate executable memory-mapped I/O verification test cases from IP-XACT or
//! CMSIS-SVD files.

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
