# Memorymap selftest for Headsail

This test case can be used to check if the executing CPU can access all memory
mapped registers.

To set this up for UART in Headsail's CI, use the following configuration .gitlab-ci.yml:

```yaml
uart_connectivity:
  <<: *rust-template
  extends: .memorymap-selftest
  variables:
    INCLUDE_PERIPHERALS: uart0,uart1
  allow_failure: true
```

## Extra configurations

### `MIN_OUTPUT={0,1}`

You can minimize output to get 10x faster runs (and CI pipelines) by setting
`export MIN_OUTPUT=1`. When using this mode however, you can only determine if
the registers can be accessed or not but **not** which register is the one
causing the problem. You'll need to run the test with `export MIN_OUTPUT=0` to
find out the name and address of the register that fails to be accessed.

### `{INCLUDE_,EXCLUDE_}SYMS_REGEX`

You can use a regex to include or exclude registers from the test case based on
their name / path. E.g., to include all registers inside HPC subsystem, except
CLINT and PLIC, you could use the following regexes:

```bash
export INCLUDE_SYMS_REGEX = "^HPC"
export EXCLUDE_SYMS_REGEX= "clint|plic"
```

Full specification for the regex format can be found here:
<https://docs.rs/regex/latest/regex/>
