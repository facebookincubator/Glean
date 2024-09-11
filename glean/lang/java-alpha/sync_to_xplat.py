#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict


import os
import shutil
import sys
import tempfile
import time
from subprocess import CalledProcessError

from xplat.glean.constants import FB_CODE_ROOT, FB_SOURCE_ROOT, XPLAT_ROOT
from xplat.glean.utils import eprint, shell_exec


DEWEY_TAG: str = "glean/lang/java-alpha"
INDEXER_JAR_FILENAME: str = "indexer-alpha.jar"
STABLE_VERSION_FILE: str = os.path.join(
    XPLAT_ROOT, "glean", "lang", "java", "stable_alpha_version.bzl"
)


def get_head_sha1_hash() -> str:
    cmd = "hg --debug id -i"
    stdout = shell_exec(cmd)
    if stdout:
        x = stdout.split("\n")
        hash_index = 0 if len(x) == 1 else 1
        return str(x[hash_index]).strip()
    else:
        raise RuntimeError("Failed to get head mercurial hash")


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


def upload_to_dewey(hash: str, indexer_jar_path: str) -> None:
    tmp_dir = "/tmp/" + str(int(time.time()))

    shell_exec("mkdir -p %s" % tmp_dir)

    cmd = "cp {src} {dst}/{name}".format(
        src=indexer_jar_path, dst=tmp_dir, name=INDEXER_JAR_FILENAME
    )
    shell_exec(cmd)

    cmd = "dewey publish --verbose --create-tag --commit {hash} --tag {tag} --location {location}".format(
        hash=hash, tag=DEWEY_TAG, location=tmp_dir
    )

    try:
        shell_exec(cmd)
    except CalledProcessError as e:
        if "glean/lang/java-alpha tag for %s is already published" % hash in e.stderr:
            # this hash has already been uploaded. We can proceed normally
            pass
        else:
            raise e


def get_xplat_stable_version() -> str:
    if os.path.isfile(STABLE_VERSION_FILE):
        with open(STABLE_VERSION_FILE, "r") as infile:
            return infile.read().strip().split("=")[1].strip()
    else:
        return ""


def update_xplat_stable_version(hash: str) -> None:
    with open(STABLE_VERSION_FILE, "w") as outfile:
        outfile.write('DEWEY_STABLE_ALPHA_VERSION = "%s"\n' % hash)


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
    head_hash = get_head_sha1_hash()
    if head_hash.endswith("+"):
        eprint("Uncomitted changes. Aborting")
        sys.exit(1)

    if get_xplat_stable_version() == head_hash:
        eprint("xplat version matches head. Nothing to do.")
        sys.exit(0)

    indexer_jar_path = build_indexer_jar()
    upload_to_dewey(head_hash, indexer_jar_path)
    update_xplat_stable_version(head_hash)

    verify_xplat_indexer()


if __name__ == "__main__":
    main()
