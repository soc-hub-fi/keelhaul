# register-selftest-generator

Memory-mapped I/O register test case generator.

## How to use

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

Build testcases.

`just build`

Build and run (**will cause segmentation fault 8-)**).

`just run`

File with test cases can usually be found inside `target/<debug | release>/build/register-selftest-<hash>/out/register_selftest.rs`.

### Example build

Build test cases from `test.svd`-file.

`SVD_PATH=$PWD/data/test.svd just build`

## Dependencies

- `cargo`
    - `curl https://sh.rustup.rs -sSf | sh`
- `just`
    - `cargo install just`
