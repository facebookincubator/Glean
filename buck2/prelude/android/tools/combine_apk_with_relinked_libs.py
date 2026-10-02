# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

"""Combine an APK/AAB with relinked native libraries.

Replaces the un-relinked .so files in the input APK/AAB with their
relinked counterparts. A JSON mapping file specifies which libraries
to replace, keyed by <abi>/<soname> (e.g. "arm64-v8a/libfoo.so").

The mapping is produced by the build system and consumed here, so the
script doesn't need to discover or infer what to replace.

Handles .so files at all possible packaging locations:
  - lib/<abi>/<soname>                        (standard)
  - assets/lib/<abi>/<soname>                 (asset libraries)
  - assets/<module>/lib/<abi>/<soname>        (Voltron module libs)
  - assets/<module>/assets/<abi>/<soname>     (Voltron module assets)
  - Inside nested ZIP/JAR files               (module containers)

Usage:
    combine_apk_with_relinked_libs.py \
        --input input.apk \
        --relinked-libs-dir /path/to/relinked_libs \
        --mapping /path/to/mapping.json \
        --output output.apk
"""

import argparse
import io
import json
import os
import shutil
import zipfile


def _build_relinked_lookup(
    mapping: list[str],
    relinked_libs_dir: str,
) -> dict[str, str]:
    """Build a lookup dict from the mapping list.

    Args:
        mapping: List of "<abi>/<soname>" strings from the build system.
        relinked_libs_dir: Base directory containing the relinked .so files.

    Returns:
        Dict mapping "<abi>/<soname>" to the absolute filesystem path.
    """
    lookup = {}
    for abi_soname in mapping:
        path = os.path.join(relinked_libs_dir, abi_soname)
        if not os.path.isfile(path):
            raise RuntimeError(
                "Relinked library not found: {} (expected at {})".format(
                    abi_soname, path
                )
            )
        lookup[abi_soname] = path
    return lookup


def _find_matching_key(
    zip_entry: str,
    relinked_lookup: dict[str, str],
) -> str | None:
    """Find the mapping key that matches a ZIP entry path.

    Keys are always <abi>/<soname> (exactly two path components), so we
    extract the last two components of the zip entry and do a direct
    dict lookup — O(1) per entry instead of O(M) linear scan.

    Note: suffix matching is intentional — the same <abi>/<soname> may appear
    at multiple packaging locations (lib/, assets/lib/, assets/<module>/lib/,
    etc.) and all occurrences should be replaced with the same relinked binary.
    """
    parts = zip_entry.rsplit("/", 2)
    if len(parts) >= 2:
        candidate = parts[-2] + "/" + parts[-1]
        if candidate in relinked_lookup:
            return candidate
    return None


def _maybe_rebuild_inner_zip(
    inner_bytes: bytes,
    relinked_lookup: dict[str, str],
) -> tuple[bytes, set[str]] | None:
    """Rebuild a nested ZIP, replacing any .so entries with relinked versions.

    Returns a tuple of (rebuilt bytes, set of replaced keys) if any
    replacements were made, or None if the inner ZIP has no matching
    entries (or is not a valid ZIP). This avoids parsing the same ZIP
    twice (once to check, once to rebuild).
    """
    try:
        buf = io.BytesIO(inner_bytes)
        with zipfile.ZipFile(buf, "r") as zin:
            has_match = any(
                _find_matching_key(info.filename, relinked_lookup)
                for info in zin.infolist()
            )
            if not has_match:
                return None
            inner_replaced: set[str] = set()
            out = io.BytesIO()
            with zipfile.ZipFile(out, "w") as zout:
                for info in zin.infolist():
                    key = _find_matching_key(info.filename, relinked_lookup)
                    if key is not None:
                        with open(relinked_lookup[key], "rb") as f:
                            zout.writestr(info, f.read())
                        inner_replaced.add(key)
                    else:
                        zout.writestr(info, zin.read(info.filename))
            return out.getvalue(), inner_replaced
    except zipfile.BadZipFile:
        return None


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Replace .so files in an APK/AAB with relinked versions."
    )
    parser.add_argument(
        "--input",
        required=True,
        help="Path to the input APK/AAB",
    )
    parser.add_argument(
        "--relinked-libs-dir",
        required=True,
        help="Path to directory containing relinked .so files (structure: <abi>/<soname>)",
    )
    parser.add_argument(
        "--mapping",
        required=True,
        help="Path to JSON file listing <abi>/<soname> entries to replace",
    )
    parser.add_argument(
        "--output",
        required=True,
        help="Path to write the combined output APK/AAB",
    )
    args = parser.parse_args()

    with open(args.mapping) as f:
        mapping = json.load(f)

    relinked_lookup = _build_relinked_lookup(mapping, args.relinked_libs_dir)

    if not relinked_lookup:
        shutil.copy2(args.input, args.output)
        return

    replaced: set[str] = set()
    with zipfile.ZipFile(args.input, "r") as zin:
        with zipfile.ZipFile(args.output, "w") as zout:
            for info in zin.infolist():
                key = _find_matching_key(info.filename, relinked_lookup)
                if key is not None:
                    with open(relinked_lookup[key], "rb") as f:
                        data = f.read()
                    zout.writestr(info, data)
                    replaced.add(key)
                elif info.filename.endswith((".jar", ".zip")):
                    inner_bytes = zin.read(info.filename)
                    result = _maybe_rebuild_inner_zip(inner_bytes, relinked_lookup)
                    if result is not None:
                        rebuilt, inner_replaced = result
                        zout.writestr(info, rebuilt)
                        replaced.update(inner_replaced)
                    else:
                        zout.writestr(info, inner_bytes)
                else:
                    zout.writestr(info, zin.read(info.filename))

    not_replaced = set(relinked_lookup.keys()) - replaced
    if not_replaced:
        raise RuntimeError(
            "Relinked libraries not found in input: {}".format(
                ", ".join(sorted(not_replaced))
            )
        )


if __name__ == "__main__":
    main()
