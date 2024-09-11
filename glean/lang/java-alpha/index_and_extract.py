#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict


import argparse
import glob
import json
import os
import shutil
import tempfile
import time
import zipfile

from xplat.glean.utils import shell_exec


def build_target_with_debug_logs(target: str) -> str:
    cmd = f"buck1 build -c javac.random_seed={int(time.time())} {target} --show-output --local"
    env = os.environ.copy()
    env["GLEAN_DEBUG_LOGGING"] = "True"
    stdout = shell_exec(cmd, env=env)
    if stdout:
        return stdout.split(" ")[1].strip()
    else:
        raise RuntimeError(f"Unable to build target {target}")


def extract_jar_to_tmp_dir(jar: str) -> str:
    tmp_dir = tempfile.mkdtemp()
    tmp_jar = os.path.join(tmp_dir, "tmp.jar")

    shutil.copyfile(jar, tmp_jar)
    with zipfile.ZipFile(tmp_jar, "r") as zip_ref:
        zip_ref.extractall(tmp_dir)

    return tmp_dir


def print_debug_logs(extracted_jar_dir: str) -> None:
    meta_inf_dir = os.path.join(extracted_jar_dir, "META-INF")
    for debug_file in glob.glob(f"{meta_inf_dir}/glean-log*.txt"):
        with open(debug_file, "r") as infile:
            print(debug_file)
            print(infile.read())
            print("\n\n")


def get_concat_index_file(extracted_jar_dir: str) -> str:
    meta_inf_dir = os.path.join(extracted_jar_dir, "META-INF")
    concat_index = []
    for index_file in glob.glob(f"{meta_inf_dir}/glean-*.json"):
        with open(index_file, "r") as infile:
            concat_index.extend(json.load(infile))

    concat_index_file = f"{extracted_jar_dir}/concat-index.json"
    with open(concat_index_file, "w") as outfile:
        json.dump(concat_index, outfile, indent=2)

    return concat_index_file


def main() -> None:
    parser = argparse.ArgumentParser(fromfile_prefix_chars="@")
    parser.add_argument(
        "target",
        help="""
            Buck target to index.
            Example: //glean/lang/java-alpha/indexer/examples/java/com/facebook/glean/glean:glean
            The target
              - should be a cache_bustable_java_library to prevent buck caching.
              - should include 'extra_arguments = ["-Xplugin:Indexer"]'
              - should include '//glean/lang/java-alpha:indexer-lib' as a dep
              - should include '//third-party-java/org.slf4j:slf4j-api' and '//third-party-java/org.slf4j:slf4j-simple' as provided_deps
            """,
    )
    parsed_args = parser.parse_args()

    target = f"{parsed_args.target}-java11"
    print(f"Building target: {target}")

    jar_path = build_target_with_debug_logs(target)
    print(f"jar with debug logs: {jar_path}")

    exctracted_jar_dir = extract_jar_to_tmp_dir(jar_path)
    concat_index_file = get_concat_index_file(exctracted_jar_dir)
    print("concat index file")
    print(concat_index_file)

    print(
        f"""
        To load this into a local db run the following commands

        cd ~/fbsource/fbcode
        mkdir -p /tmp/glean/db_root
        buck run //glean/shell:shell -- --db-root /tmp/glean/db_root

        > inside glean shell
        :load {concat_index_file}
    """
    )


if __name__ == "__main__":
    main()
