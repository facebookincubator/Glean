#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict


import hashlib
import os
import re
import shutil
import sys
import tempfile
from subprocess import CalledProcessError

from xplat.glean.constants import FB_CODE_ROOT, FB_SOURCE_ROOT, XPLAT_ROOT
from xplat.glean.utils import eprint, shell_exec


MANIFOLD_BUCKET: str = "csi_misc"
MANIFOLD_PATH: str = "tree/glean-java-alpha"
BUCK_FILE: str = os.path.join(XPLAT_ROOT, "glean", "lang", "java", "BUCK")


def has_uncommitted_changes() -> bool:
    """Check if there are uncommitted changes in the repository."""
    cmd = "hg --debug id -i"
    stdout = shell_exec(cmd)
    if stdout:
        # If the hash ends with "+", there are uncommitted changes
        return stdout.strip().endswith("+")
    else:
        raise RuntimeError("Failed to get mercurial status")


def build_indexer_jar() -> str:
    cmd = "buck build fbcode//glean/lang/java-alpha:java_alpha --show-output"
    stdout = shell_exec(cmd)
    if not stdout:
        raise RuntimeError("Failed to build indexer")

    tmp_dir = tempfile.mkdtemp()
    tmp_dir_jex_path = os.path.join(tmp_dir, "indexer-alpha.jex")
    jex_path = os.path.join(FB_CODE_ROOT, stdout.split(" ")[1].strip())

    shutil.copyfile(jex_path, tmp_dir_jex_path)
    try:
        shell_exec(f"unzip {tmp_dir_jex_path}", cwd=tmp_dir)
    except CalledProcessError as e:
        # unzip can return an exit code of 1 if one or more warning errors were encountered, but processing completed successfully anyway.
        # Ignore this return code and assume it was successfull. Verify in fbsource.verify will fail if the indexer is broken.
        if e.returncode != 1:
            raise e

    return os.path.join(tmp_dir, "java_alpha.jar")


def upload_to_manifold(indexer_jar_path: str) -> str:
    """Upload the indexer jar to Manifold and return its SHA1 hash."""
    from manifold.clients.python import ManifoldClient

    # Read and hash the file
    # lint-ignore: poor-choice-of-hash-function
    # SHA1 is required by the manifold_get BUCK macro for integrity verification
    with open(indexer_jar_path, "rb") as f:
        file_contents = f.read()
    sha1_hash = hashlib.sha1(file_contents).hexdigest()  # noqa: S324

    # Upload to Manifold (allow overwrite since we're updating the artifact)
    with ManifoldClient.get_client(MANIFOLD_BUCKET) as client:
        client.sync_put(
            MANIFOLD_PATH,
            file_contents,
            predicate=ManifoldClient.Predicates.AllowOverwrite,
        )

    eprint(
        f"Uploaded {indexer_jar_path} to manifold://{MANIFOLD_BUCKET}/{MANIFOLD_PATH}"
    )
    eprint(f"SHA1: {sha1_hash}")

    return sha1_hash


def update_buck_sha1(sha1_hash: str) -> None:  # noqa: S324
    """Update the sha1 hash in the BUCK file for the alpha indexer."""
    with open(BUCK_FILE, "r") as f:
        content = f.read()

    # Update the sha1 for indexer-alpha-artifact
    pattern = r'(name = "indexer-alpha-artifact".*?sha1 = ")[a-f0-9]+(")'
    new_content = re.sub(pattern, rf"\g<1>{sha1_hash}\g<2>", content, flags=re.DOTALL)

    if new_content == content:
        raise RuntimeError("Failed to update SHA1 in BUCK file - pattern not found")

    with open(BUCK_FILE, "w") as f:
        f.write(new_content)


def verify_xplat_indexer() -> bool:
    cmd = "buck build fbsource//xplat/glean/lang/java:verify"
    try:
        shell_exec(cmd, cwd=FB_SOURCE_ROOT)
        return True
    except CalledProcessError as e:
        eprint("Indexer failed verification")
        eprint(e.stderr)
        return False


def main() -> None:
    if has_uncommitted_changes():
        eprint("Uncommitted changes. Aborting")
        sys.exit(1)

    indexer_jar_path = build_indexer_jar()
    sha1_hash = upload_to_manifold(indexer_jar_path)
    update_buck_sha1(sha1_hash)

    verify_xplat_indexer()


if __name__ == "__main__":
    main()
