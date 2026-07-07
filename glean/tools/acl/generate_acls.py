# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

"""
Standalone tool to generate ACL JSON from scsc restricted-paths output.

Produces a JSON file mapping directory paths to their ACL group names:
{
    "dir_path": ["group_name1", "group_name2"],
    ...
}
"""

import argparse
import json
import os
import re
import subprocess
import sys
from collections import defaultdict
from pathlib import Path

# Regex to parse scsc restricted-paths output lines
SCSC_LINE_RE: re.Pattern[str] = re.compile(r"^(.+?)\s+\(ACLs:\s+REPO_REGION:(.+)\)$")


def parse_scsc_output(text: str) -> list[tuple[str, str]]:
    """
    Parse scsc restricted-paths output into (dir_path, acl_name) tuples.

    Each line has format:
        dir_path (ACLs: REPO_REGION:repos/hg/repo/=acl_name)
    """
    entries: list[tuple[str, str]] = []
    failed_lines: list[str] = []
    for line in text.strip().splitlines():
        line = line.strip()
        if not line:
            continue
        match = SCSC_LINE_RE.match(line)
        if match:
            dir_path = match.group(1).strip()
            raw_acl = match.group(2).strip()
            # Strip the repo path prefix (e.g. "repos/hg/repo/=") to get
            # just the ACL group name suffix (e.g. "alpha_acl").
            acl_name = raw_acl.rsplit("=", 1)[-1].strip() if "=" in raw_acl else raw_acl
            # An empty dir_path or acl_name means the line matched the shape but
            # not the substance; treating it as parsed would emit an ACL entry
            # keyed on "" or granting access to group "", so reject it loudly.
            if not dir_path or not acl_name:
                failed_lines.append(line)
                continue
            entries.append((dir_path, acl_name))
        else:
            failed_lines.append(line)
    # Fail loudly on any unparseable line: silently dropping a line would
    # emit an ACL config that under-restricts the affected paths, so downstream
    # indexing must never proceed on a partial parse.
    if failed_lines:
        details = "\n".join(f"  {line}" for line in failed_lines)
        raise ValueError(
            f"Failed to parse {len(failed_lines)} scsc output line(s):\n{details}"
        )
    return entries


def validate_root(root: str) -> str:
    """
    Validate the operator-supplied --root before it reaches a subprocess.

    Commands are already built as argument lists (never through a shell), so
    there is no shell-injection surface. The remaining risk is argument
    injection: a value beginning with '-' would be parsed as a flag by hg/scsc
    rather than a path. Rejecting that, and confirming the path is a real
    directory, keeps the tool from silently running against the wrong tree.
    """
    if root.startswith("-"):
        raise ValueError(f"--root must not start with '-': {root!r}")
    path = Path(root)
    if not path.is_dir():
        raise ValueError(f"--root is not an existing directory: {root!r}")
    return str(path)


def validate_repo_name(repo_name: str) -> str:
    """Reject empty or flag-like repo names before passing them to scsc/hg."""
    if not repo_name:
        raise ValueError("repo name is empty")
    if repo_name.startswith("-"):
        raise ValueError(f"repo name must not start with '-': {repo_name!r}")
    return repo_name


def get_repo_name(root: str | None = None) -> str:
    """Get the repository name from hg root."""
    if root is not None:
        root = validate_root(root)
    try:
        result = subprocess.run(
            ["hg", "root"],
            capture_output=True,
            text=True,
            check=True,
            cwd=root,
        )
        return Path(result.stdout.strip()).name
    except (subprocess.CalledProcessError, FileNotFoundError) as e:
        raise RuntimeError(f"Failed to get repo name from 'hg root': {e}") from e


def run_scsc(repo_name: str, root: str | None = None) -> str:
    """Run scsc restricted-paths and return its output."""
    repo_name = validate_repo_name(repo_name)
    cmd = ["scsc", "restricted-paths", "find", "-R", repo_name, "-B", "master"]
    if root is not None:
        root = validate_root(root)
        cmd.extend(["--root", root])

    env = {**os.environ, "SCSC_ADMIN_ENABLED": "1"}

    try:
        result = subprocess.run(
            cmd,
            capture_output=True,
            text=True,
            check=True,
            env=env,
        )
        return result.stdout
    except subprocess.CalledProcessError as e:
        raise RuntimeError(
            f"scsc command failed (exit {e.returncode}): {e.stderr}"
        ) from e


def generate_acl_json(
    entries: list[tuple[str, str]],
) -> dict[str, list[str]]:
    """
    Generate the ownership-only ACL JSON from parsed entries.

    Returns:
        {dir_path: [acl_group_name, ...], ...}
    """
    acls: dict[str, list[str]] = defaultdict(list)
    seen: dict[str, set[str]] = defaultdict(set)
    for dir_path, acl_name in entries:
        # Dedup via a per-path set (O(1) membership) while preserving first-seen
        # order in the emitted list; the list alone would be O(n) per insert.
        if acl_name not in seen[dir_path]:
            seen[dir_path].add(acl_name)
            acls[dir_path].append(acl_name)

    return dict(acls)


def write_output(result: dict[str, list[str]], output_path: str | None) -> None:
    """
    Serialize the ACL config to output_path, or stdout when it is None.

    Writes to a temp file and renames into place so a crash mid-write can never
    leave a truncated JSON file that a downstream consumer would treat as a
    complete, valid ACL config.
    """
    output_text = json.dumps(result, indent=2) + "\n"
    if output_path is None:
        sys.stdout.write(output_text)
        return

    out = Path(output_path)
    tmp = out.with_name(f"{out.name}.tmp")
    try:
        tmp.write_text(output_text)
        tmp.replace(out)
    except OSError:
        # Remove the partial temp file before surfacing the error.
        try:
            tmp.unlink()
        except OSError:
            pass
        raise


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Generate ACL JSON from scsc restricted-paths"
    )
    parser.add_argument(
        "--root",
        help="Root path to pass through to scsc --root",
    )
    parser.add_argument(
        "--repo_name",
        help="Repository name (overrides auto-detect from hg root)",
    )
    parser.add_argument(
        "--output",
        help="Output file path (default: stdout)",
    )
    parser.add_argument(
        "--scsc-output",
        dest="scsc_output",
        help="Read pre-captured scsc output from file instead of running scsc "
        "(for testing)",
    )

    args = parser.parse_args()

    try:
        root = validate_root(args.root) if args.root else None

        # Get scsc output
        if args.scsc_output:
            with open(args.scsc_output) as f:
                scsc_text = f.read()
        else:
            repo_name = args.repo_name or get_repo_name(root)
            scsc_text = run_scsc(repo_name, root)

        # Parse and generate
        entries = parse_scsc_output(scsc_text)
        if not entries:
            raise ValueError("No ACL entries parsed from scsc output")

        result = generate_acl_json(entries)

        # Write inside the try so an output failure is reported as a clear error
        # and exits non-zero, rather than surfacing as an uncaught traceback.
        write_output(result, args.output)
    except (OSError, RuntimeError, ValueError) as e:
        # Exit non-zero so callers never consume a missing or partial ACL config.
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
