python := "python3.10"

default:
    just --list --unsorted

build:
    PATH_PYTHON={{python}} cargo build

build-without-parsing:
    NO_PARSE=1 PATH_PYTHON={{python}} cargo build

run:
    PATH_PYTHON={{python}} cargo run
