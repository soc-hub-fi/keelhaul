# register-selftest-generator

Memory-mapped I/O register test case generator.

## How to use

Set following environment variables.

- `PATH_SVD`
- (optional) `INCLUDE_PERIPHERALS`
    - You must give peripheral name in lowercase.
    - You must separate peripheral names using comma `,`.
- (optional) `EXCLUDE_PERIPHERALS`
    - You must give peripheral name in lowercase.
    - You must separate peripheral names using comma `,`.

Build testcases.

`just build`

Build and run (**will cause segmentation fault 8-)**).

`just run`

## Dependencies

- `cargo`
    - `curl https://sh.rustup.rs -sSf | sh`
- `just`
    - `cargo install just`
