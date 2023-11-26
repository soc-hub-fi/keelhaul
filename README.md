# Keelhaul

Generate executable memory-mapped I/O verification test cases from IP-XACT or
CMSIS-SVD files.

## Dependencies

- Install Rust from <https://rustup.rs/>
- Install `just` (using cargo)

## SoC integration guide

Use these steps to bring up support for Keelhaul in an SoC project:

1. Create a Rust application project
    - `cargo new --bin memorymap-selftest && cd memorymap-selftest`
2. Depend on the `register-selftest` crate in Cargo.toml
    - `export SELFTEST=<path_here>/register-selftest`
    - `cargo add register-selftest --path=$SELFTEST`
3. Write a platform-specific runtime such as the one provided at [./runtimes/headsail-selftest/src/main.rs](./runtimes/headsail-selftest/src/main.rs).

    `runtimes/headsail-selftest` is a reference project as used for the preparation for the tapeout of Headsail.

4. Finally, you can  generate test cases for an SVD file and run it against the target platform:

- `export SVD_PATH=path_to.svd`
- `cargo run --release`

## Supported environment variables

Set following environment variables.

- `SVD_PATH`
- (optional) `INCLUDE_PERIPHERALS`
  - You must give peripheral name in lowercase.
  - You must separate peripheral names using comma `,`.
- (optional) `EXCLUDE_PERIPHERALS`
  - You must give peripheral name in lowercase.
  - You must separate peripheral names using comma `,`.
- (optional) `INCLUDE_SYMS_REGEX`
  - **TODO**
- (optional) `EXCLUDE_SYMS_REGEX`
  - **TODO**
- (optional) `INCLUDE_TEST_KINDS`
  - Option `read` includes register read tests.
  - Option `reset` include register reset tests.
  - Option `read_is_reset_val` **TODO**
  - You must give option in lowercase.
  - You must separate multiple options using comma `,`.
- (optional) `OUTPUT_PATH`
  - Absolute path where test case source file is written to.
- (optional) `ARCHI_PTR_SIZE`
  - Override pointer size that is normally parsed from the SVD-file.

## Further build instructions

- Make sure everything compiles:
  - `SVD_PATH=$PWD/data/test.svd cargo check`
- Build test cases.
  - `SVD_PATH=$PWD/data/test.svd just build`
- Build and run (**will cause segmentation fault 8-)**).
  - `SVD_PATH=$PWD/data/test.svd just run`

File with test cases can be found at `target/<debug | release>/build/register-selftest-<hash>/out/register_selftest.rs`.

## Contributing

We are in the process of adapting the library for more use cases across the SoC Hub project. Expect major changes to the
project architecture. The libary is an unstable target and it may be difficult to integrate changes at this point.
