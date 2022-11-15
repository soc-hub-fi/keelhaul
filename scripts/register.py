from dataclasses import dataclass
from typing import Dict, Any


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
        assert isinstance(data, dict)
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
        return f"name: {self.name}, base: {self.address_base}, cluster: {self.address_offset_cluster}, register: {self.address_offset_register}, reset: {self.value_reset}, read: {self.can_read}, write: {self.can_write}"
