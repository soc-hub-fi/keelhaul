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

| Variable              | Example value       | Description                          |
| :-:                   | :-                  | :-                                   |
| `SVD_PATH`            | $PWD/test.svd       | Source for the testable register maps |
| `INCLUDE_PERIPHERALS` | apb_uart0,apb_uart1 | Comma separated list of peripheral identifiers to include |
| `EXCLUDE_PERIPHERALS` | apb_uart0,apb_uart1 | Comma separated list of peripheral identifiers to exclude |
| `INCLUDE_SYMS_REGEX`  | "^HPC.*clint\|plic"  | Regex of peripherals to include |
| `EXCLUDE_SYMS_REGEX`  | "^HPC.*clint\|plic"  | Regex of peripherals to exclude |
| `INCLUDE_TEST_KINDS`  | read,reset,read_is_reset_val | Comma separated list of test kinds to include |
| `ARCH_PTR_BYTES`      | 8                   | Architecture width in bytes, e.g. 8 for a 64-bit processor |
| `OUTPUT_PATH`         | "test_cases.rs"     | Absolute path where test case source file is written to |

## Further build instructions

- Make sure everything compiles:
  - `SVD_PATH=$PWD/data/test.svd cargo check`
- Build test cases.
  - `SVD_PATH=$PWD/data/test.svd just build`
- Build and run (**will cause segmentation fault 8-)**).
  - `SVD_PATH=$PWD/data/test.svd just run`

File with test cases can be found at `target/<debug | release>/build/register-selftest-<hash>/out/register_selftest.rs`.

## Citing

If you intend to use this tool for an academic publication, please consider citing it:

```bibtex
@inproceedings{hamalainen2023memory,
  title={Memory Mapped I/O Register Test Case Generator for Large Systems-on-Chip},
  author={H{\"a}m{\"a}l{\"a}inen, Roni and Lunnikivi, Henri and H{\"a}m{\"a}l{\"a}inen, Timo},
  booktitle={2023 IEEE Nordic Circuits and Systems Conference (NorCAS)},
  pages={1--7},
  year={2023},
  organization={IEEE}
}
```

## Contributing

We are in the process of adapting the library for more use cases across the SoC Hub project. Expect major changes to the
project architecture. The libary is an unstable target and it may be difficult to integrate changes at this point.
