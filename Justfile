[private]
default:
    @just --list --unsorted

build:
    cargo build --bin register-selftest-generator-runner

run:
    cargo run --bin register-selftest-generator-runner

doc:
    cargo doc --no-deps

doc-open:
    cargo doc --no-deps --open

# Strict clippy.
clippy:
    PATH_SVD="$PWD/data/sysctrl.svd" cargo clippy --all -- \
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
