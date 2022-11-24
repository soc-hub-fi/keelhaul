default:
    just --list --unsorted

parse:
    PATH_SVD=data/tackle.svd PATH_OUTPUT=temp/parsed.json cargo run --bin parse

parse-with-excludes:
    PATH_EXCLUDES=data/excludes.txt PATH_SVD=data/tackle.svd PATH_OUTPUT=temp/parsed.json cargo run --bin parse

generate:
    PATH_INPUT=temp/parsed.json PATH_OUTPUT=runner/src/register_tests.rs cargo run --bin generate

build:
    just parse
    just generate
    cargo build --bin runner

# TODO: build-without-parsing
# TODO: build.rs

run:
    just build
    cargo run --bin runner
