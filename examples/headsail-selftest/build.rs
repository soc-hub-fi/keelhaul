//! This Cargo build script finds the linker on setups where using a .cargo/config.toml file would
//! be inconvenient, e.g. in a Cargo workspace.

use std::{env, fs, path};

const LINK_SCRIPT: &'static str = "memory.x";

fn main() {
    // Put link script in our output directory and ensure it's on the linker search path
    let out = &path::PathBuf::from(env::var_os("OUT_DIR").unwrap());
    fs::copy(LINK_SCRIPT, out.join(LINK_SCRIPT)).unwrap();
    println!("cargo:rustc-link-search={}", out.display());
}
