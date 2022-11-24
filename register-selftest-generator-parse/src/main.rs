//! SVD-file parser for register test generator.
//!
//! # How to use
//!
//! Provide path to SVD-file and output via environment variables.
//!
//! `PATH_SVD=data/tackle.svd PATH_OUTPUT=temp/parsed.json cargo run`
//!
//! You can also provide path to file with excluded registers.
//!
//! `PATH_EXCLUDED=data/excluded.txt PATH_SVD=data/tackle.svd PATH_OUTPUT=temp/parsed.json cargo run`

mod lib;

fn main() {
    lib::parse();
}
