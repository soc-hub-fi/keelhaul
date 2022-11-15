python := "python3.10"

default:
    just --list --unsorted

build:
    PATH_PYTHON={{python}} cargo build
