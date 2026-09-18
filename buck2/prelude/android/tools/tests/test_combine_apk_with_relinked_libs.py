# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

"""Tests for combine_apk_with_relinked_libs.py.

Covers the core replacement logic: flat ZIP .so replacement, nested ZIP/JAR
replacement, error on unreplaced libraries, and the empty-mapping passthrough.
"""

from __future__ import annotations

import io
import json
import os
import tempfile
import unittest
import zipfile

from android.tools.combine_apk_with_relinked_libs import (
    _build_relinked_lookup,
    _find_matching_key,
    _maybe_rebuild_inner_zip,
    main,
)


def _make_so_bytes(tag: str) -> bytes:
    """Return distinguishable fake .so content."""
    return f"ELF-{tag}".encode()


class BuildRelinkedLookupTest(unittest.TestCase):
    """Tests for _build_relinked_lookup."""

    def test_maps_existing_files(self) -> None:
        with tempfile.TemporaryDirectory() as d:
            abi_dir = os.path.join(d, "arm64-v8a")
            os.makedirs(abi_dir)
            so_path = os.path.join(abi_dir, "libfoo.so")
            with open(so_path, "wb") as f:
                f.write(b"data")

            result = _build_relinked_lookup(["arm64-v8a/libfoo.so"], d)
            self.assertEqual(result, {"arm64-v8a/libfoo.so": so_path})

    def test_raises_on_missing_files(self) -> None:
        with tempfile.TemporaryDirectory() as d:
            with self.assertRaises(RuntimeError):
                _build_relinked_lookup(["arm64-v8a/libmissing.so"], d)

    def test_empty_mapping(self) -> None:
        with tempfile.TemporaryDirectory() as d:
            result = _build_relinked_lookup([], d)
            self.assertEqual(result, {})


class FindMatchingKeyTest(unittest.TestCase):
    """Tests for _find_matching_key."""

    def test_exact_match(self) -> None:
        lookup = {"arm64-v8a/libfoo.so": "/path"}
        self.assertEqual(
            _find_matching_key("arm64-v8a/libfoo.so", lookup), "arm64-v8a/libfoo.so"
        )

    def test_suffix_match(self) -> None:
        lookup = {"arm64-v8a/libfoo.so": "/path"}
        self.assertEqual(
            _find_matching_key("lib/arm64-v8a/libfoo.so", lookup),
            "arm64-v8a/libfoo.so",
        )

    def test_deep_suffix_match(self) -> None:
        lookup = {"arm64-v8a/libfoo.so": "/path"}
        self.assertEqual(
            _find_matching_key("assets/module/lib/arm64-v8a/libfoo.so", lookup),
            "arm64-v8a/libfoo.so",
        )

    def test_no_match(self) -> None:
        lookup = {"arm64-v8a/libfoo.so": "/path"}
        self.assertIsNone(_find_matching_key("arm64-v8a/libbar.so", lookup))

    def test_partial_name_no_false_match(self) -> None:
        lookup = {"arm64-v8a/libfoo.so": "/path"}
        # "xarm64-v8a/libfoo.so" should NOT match because "/" separator is required
        self.assertIsNone(_find_matching_key("xarm64-v8a/libfoo.so", lookup))


class MaybeRebuildInnerZipTest(unittest.TestCase):
    """Tests for _maybe_rebuild_inner_zip."""

    def test_returns_none_when_no_match(self) -> None:
        buf = io.BytesIO()
        with zipfile.ZipFile(buf, "w") as zf:
            zf.writestr("lib/arm64-v8a/libbar.so", b"data")
        lookup = {"arm64-v8a/libfoo.so": "/path"}
        self.assertIsNone(_maybe_rebuild_inner_zip(buf.getvalue(), lookup))

    def test_returns_none_for_bad_zip(self) -> None:
        lookup = {"arm64-v8a/libfoo.so": "/path"}
        self.assertIsNone(_maybe_rebuild_inner_zip(b"not a zip", lookup))

    def test_replaces_matching_entry(self) -> None:
        original = _make_so_bytes("original")
        relinked = _make_so_bytes("relinked")

        buf = io.BytesIO()
        with zipfile.ZipFile(buf, "w") as zf:
            zf.writestr("lib/arm64-v8a/libfoo.so", original)
            zf.writestr("classes.dex", b"dex-data")

        with tempfile.TemporaryDirectory() as d:
            abi_dir = os.path.join(d, "arm64-v8a")
            os.makedirs(abi_dir)
            with open(os.path.join(abi_dir, "libfoo.so"), "wb") as f:
                f.write(relinked)

            lookup = {"arm64-v8a/libfoo.so": os.path.join(abi_dir, "libfoo.so")}
            result = _maybe_rebuild_inner_zip(buf.getvalue(), lookup)

        self.assertIsNotNone(result)
        result_bytes, replaced = result
        self.assertIn("arm64-v8a/libfoo.so", replaced)

        with zipfile.ZipFile(io.BytesIO(result_bytes), "r") as zout:
            self.assertEqual(zout.read("lib/arm64-v8a/libfoo.so"), relinked)
            self.assertEqual(zout.read("classes.dex"), b"dex-data")


class MainIntegrationTest(unittest.TestCase):
    """Integration tests for the main() entry point."""

    def _run_combine(
        self,
        tmpdir: str,
        input_entries: dict[str, bytes],
        mapping: list[str],
        relinked_files: dict[str, bytes],
    ) -> str:
        """Build input APK, mapping JSON, relinked libs dir, run main(), return output path."""
        # Create input APK
        input_path = os.path.join(tmpdir, "input.apk")
        with zipfile.ZipFile(input_path, "w") as zf:
            for name, data in input_entries.items():
                zf.writestr(name, data)

        # Create relinked libs directory
        relinked_dir = os.path.join(tmpdir, "relinked_libs")
        for rel_path, data in relinked_files.items():
            full_path = os.path.join(relinked_dir, rel_path)
            os.makedirs(os.path.dirname(full_path), exist_ok=True)
            with open(full_path, "wb") as f:
                f.write(data)

        # Create mapping JSON
        mapping_path = os.path.join(tmpdir, "mapping.json")
        with open(mapping_path, "w") as f:
            json.dump(mapping, f)

        output_path = os.path.join(tmpdir, "output.apk")

        import sys

        old_argv = sys.argv
        sys.argv = [
            "combine_apk_with_relinked_libs.py",
            "--input",
            input_path,
            "--relinked-libs-dir",
            relinked_dir,
            "--mapping",
            mapping_path,
            "--output",
            output_path,
        ]
        try:
            main()
        finally:
            sys.argv = old_argv

        return output_path

    def test_flat_so_replacement(self) -> None:
        """Standard lib/<abi>/<soname> entries are replaced."""
        original = _make_so_bytes("original")
        relinked = _make_so_bytes("relinked")

        with tempfile.TemporaryDirectory() as tmpdir:
            output = self._run_combine(
                tmpdir,
                input_entries={
                    "lib/arm64-v8a/libfoo.so": original,
                    "classes.dex": b"dex-data",
                },
                mapping=["arm64-v8a/libfoo.so"],
                relinked_files={"arm64-v8a/libfoo.so": relinked},
            )

            with zipfile.ZipFile(output, "r") as zf:
                self.assertEqual(zf.read("lib/arm64-v8a/libfoo.so"), relinked)
                self.assertEqual(zf.read("classes.dex"), b"dex-data")

    def test_nested_jar_replacement(self) -> None:
        """Libraries inside nested JARs are replaced."""
        original = _make_so_bytes("original")
        relinked = _make_so_bytes("relinked")

        inner_jar = io.BytesIO()
        with zipfile.ZipFile(inner_jar, "w") as zf:
            zf.writestr("lib/arm64-v8a/libfoo.so", original)

        with tempfile.TemporaryDirectory() as tmpdir:
            output = self._run_combine(
                tmpdir,
                input_entries={
                    "module.jar": inner_jar.getvalue(),
                    "classes.dex": b"dex-data",
                },
                mapping=["arm64-v8a/libfoo.so"],
                relinked_files={"arm64-v8a/libfoo.so": relinked},
            )

            with zipfile.ZipFile(output, "r") as zf:
                inner_bytes = zf.read("module.jar")
                with zipfile.ZipFile(io.BytesIO(inner_bytes), "r") as inner:
                    self.assertEqual(inner.read("lib/arm64-v8a/libfoo.so"), relinked)

    def test_empty_mapping_copies_input(self) -> None:
        """When the mapping is empty, the output should be a copy of the input."""
        with tempfile.TemporaryDirectory() as tmpdir:
            input_path = os.path.join(tmpdir, "input.apk")
            with zipfile.ZipFile(input_path, "w") as zf:
                zf.writestr("classes.dex", b"dex-data")

            mapping_path = os.path.join(tmpdir, "mapping.json")
            with open(mapping_path, "w") as f:
                json.dump([], f)

            # Create an empty relinked dir
            relinked_dir = os.path.join(tmpdir, "relinked_libs")
            os.makedirs(relinked_dir)

            output_path = os.path.join(tmpdir, "output.apk")

            import sys

            old_argv = sys.argv
            sys.argv = [
                "combine_apk_with_relinked_libs.py",
                "--input",
                input_path,
                "--relinked-libs-dir",
                relinked_dir,
                "--mapping",
                mapping_path,
                "--output",
                output_path,
            ]
            try:
                main()
            finally:
                sys.argv = old_argv

            with zipfile.ZipFile(output_path, "r") as zf:
                self.assertEqual(zf.read("classes.dex"), b"dex-data")

    def test_unreplaced_library_raises(self) -> None:
        """If a mapped library is not found in the input APK, a RuntimeError is raised."""
        with tempfile.TemporaryDirectory() as tmpdir:
            with self.assertRaises(RuntimeError):
                self._run_combine(
                    tmpdir,
                    input_entries={
                        "classes.dex": b"dex-data",
                    },
                    mapping=["arm64-v8a/libfoo.so"],
                    relinked_files={"arm64-v8a/libfoo.so": b"relinked"},
                )
