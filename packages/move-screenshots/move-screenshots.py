#!/usr/bin/env python

from pathlib import Path
import os.path
import re

def main():
    root = Path(os.path.expanduser("~/Pictures/Screenshots"))
    file_pattern = re.compile(r"Screenshot from (\d{4}-\d{2})", re.IGNORECASE)

    for file in root.glob("Screenshot from *.png", case_sensitive=False):
        if not (match := file_pattern.match(file.name)):
            print(f"Unknown file {file}")
            continue

        folder = root / match.group(1)
        folder.mkdir(parents=True, exist_ok=True)

        print(f"Moving {file} to {folder}")
        file.rename(folder / file.name)


if __name__ == '__main__':
    main()
