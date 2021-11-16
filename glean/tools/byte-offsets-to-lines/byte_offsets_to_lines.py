#!/usr/bin/env python3
# Copyright (c) Facebook, Inc. and its affiliates.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict


import argparse
from typing import List, Tuple


def read_file(path: str, offsets: List[int]) -> None:
    offsets.sort()
    relative_offsets = [offsets[n] - offsets[n - 1] for n in range(1, len(offsets))]
    relative_offsets.insert(0, offsets[0])

    offsets: Tuple[int, int] = zip(offsets, relative_offsets)

    with open(path, "r") as infile:
        current_line = 1
        current_column = 1
        for offset in offsets:
            absolute_offset, relative_offset = offset

            chunk = infile.read(relative_offset).split("\n")
            last_line = chunk[-1]
            num_new_lines = len(chunk) - 1

            if num_new_lines != 0:
                current_line += num_new_lines
                current_column = 1

            current_column += len(last_line)

            print(f"{absolute_offset} -> {current_line}:{current_column}")


def main() -> None:
    parser = argparse.ArgumentParser(fromfile_prefix_chars="@")
    parser.add_argument("file", help="File path to read")
    parser.add_argument(
        "offsets", nargs="+", type=int, help="Byte offsets to convert to line numbers"
    )

    parsed_args = parser.parse_args()

    read_file(parsed_args.file, parsed_args.offsets)


if __name__ == "__main__":
    main()
