[private]
default:
    @just --list --unsorted

build:
    cargo build --example=run_every_test

check:
    cargo check --example=run_every_test

run:
    cargo run --example=run_every_test

doc:
    cargo doc --no-deps

doc-open:
    cargo doc --no-deps --open
