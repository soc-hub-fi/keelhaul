# Keelhaul

Generate executable memory-mapped I/O verification test cases from IP-XACT or
CMSIS-SVD files.

## Dependencies

- Make sure to run `git submodule update --init` or equivalent
- Install Rust from <https://rustup.rs/>
- Install `just`, a command runner for access to convenient build job automation
  - `cargo install just`

## Getting started - try the CLI

The easiest way to use the tool is to run the CLI and explore what it can do:

```sh
cd keelhaul-cli

cargo run -- --help
```

N.b., SoC Hub chip verification so far has used the custom parser available for all subcommands via
`--use-legacy`.

## SoC integration guide (register-selftest)

See also. [register-selftest](./register-selftest/README.md).

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

## Supported models

Supported model formats:

- [x] [CMSIS-SVD](https://www.keil.com/pack/doc/CMSIS/SVD/html/index.html)

Support is planned for:

- [ ] [IEEE 1685-2014](https://standards.ieee.org/ieee/1685/5834/) (IP-XACT 2014)
- [ ] [IEEE 1685-2022](https://standards.ieee.org/ieee/1685/10583/) (IP-XACT 2022)

Support is not planned for:

- [IEEE 1685-2009](https://standards.ieee.org/ieee/1685/4013/) (IP-XACT 2009)

## Limitations

- While CMSIS-SVD allows specifying otherwise, the `keelhaul` currently assumes
  that each individual address uniquely selects exactly 8-bits, as this is the
  most common case in contemporary architectures. This limitation could be
  lifted.

## Contributing

We are in the process of adapting the library for more use cases across the SoC Hub project. Expect major changes to the
project architecture. The libary is an unstable target and it may be difficult to integrate changes at this point.

## VS Code settings for rust-analyzer

While we support the `register-selftest", the environment variable based configuration utility, you
will need this in your VS Code settings to allow the linter to run without failing.

```json
{
    "rust-analyzer.cargo.extraEnv": {
        "ARCH_PTR_BYTES": "4",
        "SVD_PATH": "/abs/path/to/keelhaul/data/test.svd",
    },
    // Use clippy
    "rust-analyzer.check.command": "clippy",
}
```
