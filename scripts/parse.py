from pathlib import Path
import logging
import sys
from typing import List, Dict
from dataclasses import dataclass, asdict
import json

from bs4 import BeautifulSoup


@dataclass
class Register:
    name: str
    address_base: int
    address_offset_cluster: int
    address_offset_register: int
    value_reset: int
    can_read: bool
    can_write: bool

    def __str__(self) -> str:
        return f"name: {self.name}, base: {self.address_base}, cluster: {self.address_offset_cluster}, register: {self.address_offset_register}, reset: {self.value_reset}, read: {self.can_read}, write: {self.can_write}"

    def __eq__(self, other: object) -> bool:
        assert isinstance(other, Register)
        if self.address_base != other.address_base:
            return False
        if self.address_base != other.address_base:
            return False
        if self.address_base != other.address_base:
            return False
        if self.address_base != other.address_base:
            return False
        if self.address_base != other.address_base:
            return False
        if self.address_base != other.address_base:
            return False
        return True


if __name__ == "__main__":
    logging.basicConfig(
        format="",
        level=logging.DEBUG,
        encoding="utf-8",
        stream=sys.stdout,
    )
    path_svd = Path(f"{__file__}/../../data/tackle.svd").resolve()
    if not path_svd.exists():
        sys.exit(f"Path to SVD does not exist. {path_svd}")
    if not path_svd.is_file():
        sys.exit(f"Path to SVD does not point to a file. {path_svd}")
    contents = path_svd.read_text()
    soup = BeautifulSoup(contents, "xml")
    registers: List[Register] = list()
    addresses: Dict[int, str] = dict()
    for peripheral in soup.find_all("peripheral"):
        address_base = peripheral.find("baseAddress").text
        address_base = int(address_base, 16)
        for cluster in peripheral.find_all("cluster"):
            address_offset_cluster = cluster.find("addressOffset").text
            address_offset_cluster = int(address_offset_cluster, 16)
            for register in cluster.find_all("register"):
                name = register.find("name").text
                if "(" in name:
                    logging.warning(
                        f'Register {name}\'s name contains illegal character: "(". This characted is removed.'
                    )
                    name = name.replace("(", "")
                if ")" in name:
                    logging.warning(
                        f'Register {name}\'s name contains illegal character: ")". This characted is removed.'
                    )
                    name = name.replace(")", "")
                value_reset = register.find("resetValue").text
                value_reset = int(value_reset, 16)
                address_offset_register = register.find("addressOffset").text
                address_offset_register = int(address_offset_register, 16)
                access = register.find("access")
                if not access:
                    logging.warning(f"Register {name} does not have access value.")
                    access = "read-write"
                else:
                    access = access.text
                match access:
                    case "read-write" | "read-writeOnce":
                        can_read = True
                        can_write = True
                    case "read-only":
                        can_read = True
                        can_write = False
                    case "write-only":
                        can_read = False
                        can_write = True
                    case _:
                        sys.exit(f"Invalid register access value: {access}")
                full_address = (
                    address_base + address_offset_cluster + address_offset_register
                )
                if full_address in addresses.keys():
                    logging.warning(
                        f"Register {name}'s full address is already taken by register {addresses[full_address]}. Ignoring this register."
                    )
                else:
                    addresses[full_address] = name
                    registers.append(
                        Register(
                            name=name,
                            address_base=address_base,
                            address_offset_cluster=address_offset_cluster,
                            address_offset_register=address_offset_register,
                            value_reset=value_reset,
                            can_read=can_read,
                            can_write=can_write,
                        )
                    )
    logging.info(f"Found {len(registers)} registers.")
    path_output = Path(f"{__file__}") / Path("./temp/parsed.json").resolve()
    path_output.parent.mkdir(exist_ok=True, parents=True)
    registers_as_dicts = [asdict(r) for r in registers]
    with path_output.open("w") as stream:
        json.dump(registers_as_dicts, stream)
