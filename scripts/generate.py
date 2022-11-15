from pathlib import Path
import logging
import sys
from typing import List, Dict, Any
from dataclasses import dataclass
import json


@dataclass
class Register:
    name: str
    address_base: int
    address_offset_cluster: int
    address_offset_register: int
    value_reset: int
    can_read: bool
    can_write: bool

    @staticmethod
    def from_dict(data: Dict[str, Any]) -> "Register":
        return Register(
            name=str(data["name"]),
            address_base=int(data["address_base"]),
            address_offset_cluster=int(data["address_offset_cluster"]),
            address_offset_register=int(data["address_offset_register"]),
            value_reset=int(data["value_reset"]),
            can_read=bool(data["can_read"]),
            can_write=bool(data["can_write"]),
        )

    def __str__(self) -> str:
        return f"base: {self.address_base}, cluster: {self.address_offset_cluster}, register: {self.address_offset_register}, reset: {self.value_reset}, read: {self.can_read}, write: {self.can_write}"


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
        line = f"#[allow(non_snake_case)] pub fn {function_name}() -> u32 {{0}}\n"
        output.append(line)
        function_names.append(function_name)
    function_names_combined = ",".join(function_names)
    path_output = Path(f"{__file__}/../../src/register_tests.rs").resolve()
    with path_output.open("w") as stream:
        stream.writelines(output)
        stream.write(
            f"pub static FUNCTIONS: [fn()->u32;{len(function_names)}] = [{function_names_combined}];"
        )
    logging.info(f"Wrote {len(function_names)} test cases.")
