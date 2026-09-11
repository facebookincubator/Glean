# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

"""
Generate Glean's ACL config from the source control restricted paths.

Produces a JSON file mapping directory paths to their ACL group names:
{
    "dir_path": ["group_name1", "group_name2"],
    ...
}
"""

from __future__ import annotations

import argparse
import asyncio
import json
import subprocess
import sys
from pathlib import Path

from scm.service.thrift.source_control.thrift_clients import SourceControlService
from scm.service.thrift.source_control.thrift_types import (
    CommitFindRestrictedPathsParams,
    CommitId,
    CommitIdentityScheme,
    CommitSpecifier,
    RepoResolveBookmarkParams,
    RepoSpecifier,
)
from servicerouter.python.async_client import get_sr_client
from servicerouter.python.client_params import ClientParams

SCS_TIER = "mononoke-scs-server"
CLIENT_ID = "glean-generate-acls"
TIMEOUT_MS = 300_000
BOOKMARK = "master"


def acl_group_name(path: str, path_acl: str) -> str:
    """
    Extract the group name Glean stores from a full repo-region ACL identity.

    SCS returns the ACL as a MononokeIdentity, e.g.
    "REPO_REGION:repos/hg/fbsource/=gradient". Glean stores only the bare
    region name ("gradient"); the server re-prepends the type and repo prefix
    when it checks membership (see glean/facebook/acl/AclResolve.h).
    """
    name = path_acl.rsplit("=", 1)[-1].strip()
    # A restriction root with no usable group name would silently widen access,
    # so surface it rather than emitting an empty entry.
    if not name:
        raise ValueError(f"empty ACL group name for path {path!r}")
    return name


async def fetch_acl_config(repo_name: str) -> dict[str, list[str]]:
    """
    Stream every restriction root in the repo from SCS as a path -> group
    names mapping.

    `check_permissions` is left off: this builds the config describing which
    groups guard which paths, which is independent of whoever runs the tool.
    Asking for permission checks would additionally filter by the caller's own
    access and produce a config that varies by who generated it.
    """
    repo = RepoSpecifier(name=repo_name)
    async with get_sr_client(
        SourceControlService,
        tier=SCS_TIER,
        params=(
            ClientParams()
            .setClientId(CLIENT_ID)
            .setShardManagerDomain(repo_name)
            .setProcessingTimeoutMs(TIMEOUT_MS)
            .setOverallTimeoutMs(TIMEOUT_MS)
        ),
    ) as client:
        commit_id = await resolve_bookmark(client, repo)
        params = CommitFindRestrictedPathsParams(roots=set())
        _, stream = await client.commit_find_restricted_paths(
            CommitSpecifier(repo=repo, id=commit_id), params
        )
        return {
            item.path: [acl_group_name(item.path, path_acl) for path_acl in item.acls]
            async for item in stream
        }


async def resolve_bookmark(
    client: SourceControlService.Async,
    repo: RepoSpecifier,
) -> CommitId:
    """Resolve the bookmark to the commit to read restrictions from."""
    response = await client.repo_resolve_bookmark(
        repo,
        RepoResolveBookmarkParams(
            bookmark_name=BOOKMARK,
            identity_schemes={CommitIdentityScheme.HG},
        ),
    )
    if not response.exists or not response.ids:
        raise RuntimeError(f"bookmark {BOOKMARK!r} not found in {repo.name}")
    resolved = response.ids.get(CommitIdentityScheme.HG)
    if resolved is None:
        raise RuntimeError(f"bookmark {BOOKMARK!r} has no hg commit id")
    return resolved


def validate_root(root: str) -> str:
    """
    Validate the operator-supplied --root before it reaches a subprocess.

    Commands are built as argument lists (never through a shell), so there is
    no shell-injection surface. The remaining risk is argument injection: a
    value beginning with '-' would be parsed as a flag by hg rather than a
    path. Rejecting that, and confirming the path is a real directory, keeps
    the tool from silently running against the wrong tree.
    """
    if root.startswith("-"):
        raise ValueError(f"--root must not start with '-': {root!r}")
    path = Path(root)
    if not path.is_dir():
        raise ValueError(f"--root is not an existing directory: {root!r}")
    return str(path)


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


def write_output(result: dict[str, list[str]], output_path: str | None) -> None:
    """
    Serialize the ACL config to output_path, or stdout when it is None.

    Writes to a temp file and renames into place so a crash mid-write can never
    leave a truncated JSON file that a downstream consumer would treat as a
    complete, valid ACL config.
    """
    output_text = json.dumps(result, indent=2, sort_keys=True) + "\n"
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
        description="Generate Glean's ACL config from source control restricted paths"
    )
    parser.add_argument(
        "--root",
        help="Repo checkout to auto-detect the repo name from",
    )
    parser.add_argument(
        "--repo_name",
        help="Repository name (overrides auto-detect from hg root)",
    )
    parser.add_argument(
        "--output",
        help="Output file path (default: stdout)",
    )

    args = parser.parse_args()

    try:
        root = validate_root(args.root) if args.root else None
        repo_name = args.repo_name or get_repo_name(root)
        result = asyncio.run(fetch_acl_config(repo_name))

        if not result:
            raise ValueError("SCS returned no restricted paths")

        # Write inside the try so an output failure is reported as a clear error
        # and exits non-zero, rather than surfacing as an uncaught traceback.
        write_output(result, args.output)
    except Exception as e:
        # Broad on purpose: the SCS call raises Thrift transport and application
        # errors that share no common base with OSError/RuntimeError/ValueError,
        # and every failure here means "no usable ACL config", so they all get
        # the same clean message. Exit non-zero so callers never consume a
        # missing or partial ACL config.
        print(f"Error: {type(e).__name__}: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
