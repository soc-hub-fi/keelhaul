"""Get absolute path to newest file with generated testcases

# How to use?

python3 scripts/find_testcases.py
"""

import os
from typing import List
from pathlib import Path

if __name__ == "__main__":
    path_root = Path.cwd()
    possible_paths: List[Path] = list()
    for path in path_root.glob("**/*"):
        if path.name == "register_selftest.rs":
            possible_paths.append(path)
    if len(possible_paths) == 0:
        exit(-1)
    newest_path = possible_paths[0]
    newest_timestamp = os.path.getmtime(newest_path)
    for path in possible_paths[1:]:
        path_timestamp = os.path.getmtime(path)
        if newest_timestamp < path_timestamp:
            newest_path = path
            newest_timestamp = path_timestamp
    print(f"{newest_path}")
