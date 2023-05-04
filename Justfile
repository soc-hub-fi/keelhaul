[private]
default:
    @just --list --unsorted

build:
    cargo build --example=run_every_test

build-release:
    cargo build --example=run_every_test --release

check:
    cargo check --example=run_every_test

run:
    cargo run --example=run_every_test

test:
    cargo test -p register-test-generator

doc:
    cargo doc --no-deps

doc-open:
    cargo doc --no-deps --open

test-svd:
    SVD_PATH=$PWD/data/svd/test.svd just build

test-ipxact:
    IPXACT_PATH=$PWD/data/ipxact/test1.xml just build
    IPXACT_PATH=$PWD/data/ipxact/test2.xml just build

test-svd-and-ipxact:
    just test-svd
    just test-ipxact

# Strict clippy.
clippy:
    cargo clippy --all -- \
        -W clippy::all \
        -W clippy::pedantic \
        -W clippy::restriction \
        -W clippy::nursery \
        -W clippy::cargo \
        -D warnings \
        -A clippy::implicit_return \
        -A clippy::arithmetic_side_effects \
        -A clippy::integer_arithmetic \
        -A clippy::panic \
        -A clippy::print_stdout \
        -A clippy::blanket_clippy_restriction_lints \
        -A clippy::cargo_common_metadata \
        -A clippy::single-char-lifetime-names \
        -A clippy::unwrap_used \
        -A clippy::expect_used \
        -A clippy::option_if_let_else \
        -A clippy::unnecessary_safety_comment \
        -A clippy::panic_in_result_fn \
        -A clippy::indexing_slicing \
        -A clippy::pattern_type_mismatch \
        -A clippy::or_fun_call \
        -A clippy::std_instead_of_core \
        -A clippy::wildcard_enum_match_arm \
        -A clippy::redundant_closure_for_method_calls
