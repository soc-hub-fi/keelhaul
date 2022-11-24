default:
    just --list --unsorted

parse:
    PATH_SVD=data/tackle.svd PATH_JSON=temp/parsed.json cargo run --bin parse

parse-with-excludes:
    PATH_EXCLUDES=data/excludes.txt PATH_SVD=data/tackle.svd PATH_JSON=temp/parsed.json cargo run --bin parse

generate:
    PATH_JSON=temp/parsed.json PATH_OUTPUT=runner/src/register_tests.rs cargo run --bin generate

# TODO: build-without-parsing
# TODO: build.rs

build:
    PATH_SVD=../data/tackle.svd PATH_JSON=../temp/parsed.json PATH_OUTPUT=src/register_tests.rs cargo build --bin runner

run:
    PATH_SVD=../data/tackle.svd PATH_JSON=../temp/parsed.json PATH_OUTPUT=src/register_tests.rs cargo run --bin runner

doc:
    cargo doc --no-deps

doc-open:
    cargo doc --no-deps --open
