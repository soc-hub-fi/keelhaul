use register_selftest_generator;
use register_selftest_generator_parse;

fn main() {
    println!("cargo:rerun-if-changed=src");
    println!("cargo:rerun-if-changed=build.rs");

    println!("cargo:rerun-if-env-changed=PATH_SVD");
    println!("cargo:rerun-if-env-changed=PATH_JSON");
    println!("cargo:rerun-if-env-changed=PATH_OUTPUT");

    register_selftest_generator_parse::parse();
    register_selftest_generator::generate();
}
