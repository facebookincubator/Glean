# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import json
import os
import tempfile
import unittest
from pathlib import Path
from unittest.mock import MagicMock, patch

from glean.tools.acl.generate_acls import (
    acl_group_name,
    get_repo_name,
    validate_root,
    write_output,
)


class AclGroupNameTest(unittest.TestCase):
    def test_strips_repo_region_prefix(self) -> None:
        self.assertEqual(
            acl_group_name("dir", "REPO_REGION:repos/hg/fbsource/=gradient"),
            "gradient",
        )

    def test_keeps_the_suffix_after_the_last_equals(self) -> None:
        # Regression test for the scraped-CLI bug this tool replaced: the old
        # regex captured the trailing ", request group: ..." as part of the name.
        self.assertEqual(
            acl_group_name("dir", "REPO_REGION:repos/hg/fbsource/=titan_expansion"),
            "titan_expansion",
        )

    def test_bare_value_is_used_verbatim(self) -> None:
        self.assertEqual(acl_group_name("dir", "alpha"), "alpha")

    def test_empty_name_raises(self) -> None:
        with self.assertRaises(ValueError):
            acl_group_name("dir", "REPO_REGION:repos/hg/fbsource/=")


class ValidateRootTest(unittest.TestCase):
    def test_accepts_existing_directory(self) -> None:
        with tempfile.TemporaryDirectory() as d:
            self.assertEqual(validate_root(d), str(Path(d)))

    def test_rejects_leading_dash(self) -> None:
        with self.assertRaises(ValueError):
            validate_root("--malicious-flag")

    def test_rejects_nonexistent_directory(self) -> None:
        with self.assertRaises(ValueError):
            validate_root("/this/path/should/not/exist/xyz")

    def test_rejects_file_that_is_not_a_directory(self) -> None:
        with tempfile.NamedTemporaryFile() as f:
            with self.assertRaises(ValueError):
                validate_root(f.name)


class WriteOutputTest(unittest.TestCase):
    def test_writes_json_to_file(self) -> None:
        result = {"dir": ["a", "b"]}
        with tempfile.TemporaryDirectory() as d:
            out = os.path.join(d, "acls.json")
            write_output(result, out)
            with open(out) as f:
                self.assertEqual(json.load(f), result)

    def test_leaves_no_temp_file_behind(self) -> None:
        with tempfile.TemporaryDirectory() as d:
            out = os.path.join(d, "acls.json")
            write_output({"dir": ["a"]}, out)
            self.assertEqual(os.listdir(d), ["acls.json"])

    def test_stdout_when_no_path(self) -> None:
        with patch("sys.stdout") as mock_stdout:
            write_output({"dir": ["a"]}, None)
            mock_stdout.write.assert_called_once()

    def test_cleans_up_temp_file_on_write_failure(self) -> None:
        # A write into a nonexistent directory fails; the partial temp file
        # must not be left behind and the error must propagate.
        bad_path = "/this/dir/does/not/exist/acls.json"
        with self.assertRaises(OSError):
            write_output({"dir": ["a"]}, bad_path)


class GetRepoNameTest(unittest.TestCase):
    @patch("glean.tools.acl.generate_acls.subprocess.run")
    def test_extracts_basename_from_hg_root(self, mock_run: MagicMock) -> None:
        mock_run.return_value = MagicMock(stdout="/data/users/me/fbsource\n")
        self.assertEqual(get_repo_name(), "fbsource")

    @patch("glean.tools.acl.generate_acls.subprocess.run")
    def test_wraps_subprocess_failure(self, mock_run: MagicMock) -> None:
        mock_run.side_effect = FileNotFoundError("hg not found")
        with self.assertRaises(RuntimeError):
            get_repo_name()
