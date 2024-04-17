# register-selftest

A shim for Keelhaul, providing an environment variable based configuration interface.

## SoC integration guide

See also. [register-selftest](./register-selftest/README.md).

Use these steps to bring up support for Keelhaul in an SoC project:

1. Create a Rust application project
    - `cargo new --bin memorymap-selftest && cd memorymap-selftest`
2. Depend on the `register-selftest` crate in Cargo.toml
    - `export SELFTEST=<path_here>/register-selftest`
    - `cargo add register-selftest --path=$SELFTEST`
3. Write a platform-specific runtime such as the one provided at [./examples/headsail-selftest/src/main.rs](./examples/headsail-selftest/src/main.rs).

    `examples/headsail-selftest` is a reference project as used for the preparation for the tapeout of Headsail.

4. Finally, you can  generate test cases for an SVD file and run it against the target platform:

- `export SVD_PATH=path_to.svd`
- `cargo run --release`

## Supported environment variables

| Variable              | Example value       | Description                          |
| :-:                   | :-                  | :-                                   |
| `SVD_PATH`            | $PWD/test.svd       | Source for the testable register maps |
| `INCLUDE_PERIPHERALS` | apb_uart0,apb_uart1 | Comma separated list of peripheral identifiers to include |
| `EXCLUDE_PERIPHERALS` | apb_uart0,apb_uart1 | Comma separated list of peripheral identifiers to exclude |
| `INCLUDE_SYMS_REGEX`  | "^HPC.*clint\|plic"  | Regex of peripherals to include |
| `INCLUDE_TEST_KINDS`  | read,reset,read_is_reset_val | Comma separated list of test kinds to include |
| `ARCH_PTR_BYTES`      | 8                   | Architecture width in bytes, e.g. 8 for a 64-bit processor |
| `OUTPUT_PATH`         | "test_cases.rs"     | Absolute path where test case source file is written to |
