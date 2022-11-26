default:
    just --list --unsorted

build:
    cargo build --bin register-selftest-generator-runner

run:
    cargo run --bin register-selftest-generator-runner

doc:
    cargo doc --no-deps

doc-open:
    cargo doc --no-deps --open
