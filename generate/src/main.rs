//! Register test case generator.
//!
//! # How to use
//!
//! Provide path to parser result and output via environment variables.
//!
//! `PATH_INPUT=temp/parsed.json PATH_OUTPUT=runner/src/register_tests.rs cargo run`

mod lib;

fn main() {
    lib::generate();
}
