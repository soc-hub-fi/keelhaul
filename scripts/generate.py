from pathlib import Path
import logging
import sys
from typing import List
import json

from register import Register


if __name__ == "__main__":
    logging.basicConfig(
        format="",
        level=logging.DEBUG,
        encoding="utf-8",
        stream=sys.stdout,
    )
    path_parsed = Path(f"{__file__}/../../temp/parsed.json").resolve()
    with path_parsed.open("r") as stream:
        registers = json.load(stream)
    registers = [Register.from_dict(r) for r in registers]
    output: List[str] = list()
    function_names: List[str] = list()
    for register in registers:
        address = (
            register.address_base
            + register.address_offset_cluster
            + register.address_offset_register
        )
        function_name = f"test_{register.name}_{address}"
        statements: List[str] = list()
        statements.append(
            f"#[allow(unused)] let address: *mut u32 = {address} as *mut u32;"
        )
        if register.can_read:
            statements.append(f"let _ = unsafe{{ read_volatile(address) }};")
        if register.can_write:
            statements.append(f"let reset_value = {register.value_reset};")
            statements.append(f"unsafe {{ write_volatile(address, reset_value); }}")
        statements_combined = "".join(statements)
        statements_combined = f"{statements_combined} 0"
        line = f"#[allow(non_snake_case)] pub fn {function_name}() -> u32 {{{statements_combined}}}\n"
        output.append(line)
        function_names.append(function_name)
    function_names_combined = ",".join(function_names)
    path_output = Path(f"{__file__}/../../src/register_tests.rs").resolve()
    with path_output.open("w") as stream:
        stream.write(f"use core::ptr::read_volatile;\n")
        stream.write(f"use core::ptr::write_volatile;\n")
        stream.writelines(output)
        stream.write(
            f"pub static FUNCTIONS: [fn()->u32;{len(function_names)}] = [{function_names_combined}];"
        )
    logging.info(f"Wrote {len(function_names)} test cases.")
