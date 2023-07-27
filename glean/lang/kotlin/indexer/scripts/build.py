#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree.

import os
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path

# get buck cell root
def getBuckCellRoot():
    return subprocess.check_output(["buck", "root"]).decode("utf-8").strip()


def getOutputDir():
    # Let's make this script run locally; if no output is defined, just dump
    # the output to a rando temp location.
    output_dir = os.environ.get("MSDK_OUTPUT_DIR")
    if not output_dir:
        output_dir = tempfile.mkdtemp()
    return Path(output_dir)


def getBuildCommand(output_dir):
    mode = ["@mode/mac"] if sys.platform == "darwin" else []
    buildCommand = (
        ["buck", "build"]
        + mode
        + ["fbcode//glean/lang/kotlin:indexer", "--show-full-output"]
    )

    return buildCommand


def main():
    fbcodeRoot = getBuckCellRoot()
    output_dir = getOutputDir()
    build_command = getBuildCommand(output_dir)

    try:
        result = subprocess.check_output(build_command, cwd=fbcodeRoot)

        result = result.decode("utf-8").split(" ")[-1]
        result = result.replace("\n", "")

        shutil.copy(result, output_dir)

        # This step isn't necessary for MSDK; it's to tell the developer, when building locally
        # where to check for output artifacts in case the script made a tmp output.
        print(output_dir, file=sys.stderr)

    except Exception as e:
        # This step isn't necessary for MSDK; it's to clean up after local testing.
        shutil.rmtree(output_dir)
        raise e


if __name__ == "__main__":
    sys.exit(main())
