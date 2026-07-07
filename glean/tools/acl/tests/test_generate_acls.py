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
    generate_acl_json,
    get_repo_name,
    parse_scsc_output,
    run_scsc,
    validate_repo_name,
    validate_root,
    write_output,
)


class ParseScscOutputTest(unittest.TestCase):
    def test_parses_single_line(self) -> None:
        text = "foo/bar (ACLs: REPO_REGION:repos/hg/fbsource/=alpha_acl)"
        self.assertEqual(
            parse_scsc_output(text),
            [("foo/bar", "alpha_acl")],
        )

    def test_strips_repo_path_prefix_from_acl(self) -> None:
        # The ACL name is the suffix after the final '=', regardless of the
        # repo path prefix that precedes it.
        text = "a/b (ACLs: REPO_REGION:repos/hg/some/deep/repo/=my_group)"
        self.assertEqual(parse_scsc_output(text), [("a/b", "my_group")])

    def test_acl_without_equals_is_used_verbatim(self) -> None:
        text = "a/b (ACLs: REPO_REGION:bare_acl)"
        self.assertEqual(parse_scsc_output(text), [("a/b", "bare_acl")])

    def test_ignores_blank_lines_and_surrounding_whitespace(self) -> None:
        text = (
            "\n"
            "  foo (ACLs: REPO_REGION:repos/hg/r/=g1)  \n"
            "\n"
            "bar (ACLs: REPO_REGION:repos/hg/r/=g2)\n"
        )
        self.assertEqual(
            parse_scsc_output(text),
            [("foo", "g1"), ("bar", "g2")],
        )

    def test_dir_path_with_spaces(self) -> None:
        # The non-greedy dir_path capture must still grab a path containing
        # spaces up to the " (ACLs: ...)" marker.
        text = "some dir/with space (ACLs: REPO_REGION:repos/hg/r/=g)"
        self.assertEqual(
            parse_scsc_output(text),
            [("some dir/with space", "g")],
        )

    def test_unparseable_line_raises(self) -> None:
        text = "this line does not match the expected format"
        with self.assertRaises(ValueError):
            parse_scsc_output(text)

    def test_partial_parse_raises_and_names_bad_lines(self) -> None:
        text = "good (ACLs: REPO_REGION:repos/hg/r/=g)\ntotally bogus line\n"
        with self.assertRaises(ValueError) as ctx:
            parse_scsc_output(text)
        self.assertIn("totally bogus line", str(ctx.exception))

    def test_empty_acl_name_raises(self) -> None:
        # Matches the shape but the ACL name after '=' is empty -- must not
        # silently emit an entry granting access to group "".
        text = "a/b (ACLs: REPO_REGION:repos/hg/r/=)"
        with self.assertRaises(ValueError):
            parse_scsc_output(text)


class GenerateAclJsonTest(unittest.TestCase):
    def test_groups_multiple_acls_per_dir(self) -> None:
        entries = [("dir", "a"), ("dir", "b")]
        self.assertEqual(generate_acl_json(entries), {"dir": ["a", "b"]})

    def test_deduplicates_repeated_acls(self) -> None:
        entries = [("dir", "a"), ("dir", "a"), ("dir", "b"), ("dir", "a")]
        self.assertEqual(generate_acl_json(entries), {"dir": ["a", "b"]})

    def test_preserves_first_seen_order(self) -> None:
        entries = [("dir", "b"), ("dir", "a"), ("dir", "b")]
        self.assertEqual(generate_acl_json(entries), {"dir": ["b", "a"]})

    def test_separate_dirs_are_independent(self) -> None:
        entries = [("d1", "a"), ("d2", "a"), ("d2", "b")]
        self.assertEqual(
            generate_acl_json(entries),
            {"d1": ["a"], "d2": ["a", "b"]},
        )

    def test_empty_entries_produce_empty_dict(self) -> None:
        self.assertEqual(generate_acl_json([]), {})


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


class ValidateRepoNameTest(unittest.TestCase):
    def test_accepts_normal_name(self) -> None:
        self.assertEqual(validate_repo_name("fbsource"), "fbsource")

    def test_rejects_empty(self) -> None:
        with self.assertRaises(ValueError):
            validate_repo_name("")

    def test_rejects_leading_dash(self) -> None:
        with self.assertRaises(ValueError):
            validate_repo_name("-R")


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


class RunScscTest(unittest.TestCase):
    @patch("glean.tools.acl.generate_acls.subprocess.run")
    def test_sets_admin_env_and_builds_command(self, mock_run: MagicMock) -> None:
        mock_run.return_value = MagicMock(stdout="output")
        out = run_scsc("fbsource")
        self.assertEqual(out, "output")
        args, kwargs = mock_run.call_args
        self.assertEqual(
            args[0],
            ["scsc", "restricted-paths", "find", "-R", "fbsource", "-B", "master"],
        )
        self.assertEqual(kwargs["env"]["SCSC_ADMIN_ENABLED"], "1")

    @patch("glean.tools.acl.generate_acls.subprocess.run")
    def test_appends_validated_root(self, mock_run: MagicMock) -> None:
        mock_run.return_value = MagicMock(stdout="output")
        with tempfile.TemporaryDirectory() as d:
            run_scsc("fbsource", d)
            args, _ = mock_run.call_args
            self.assertIn("--root", args[0])
            self.assertIn(str(Path(d)), args[0])

    def test_rejects_flag_like_repo_name(self) -> None:
        with self.assertRaises(ValueError):
            run_scsc("--root=/etc")


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
