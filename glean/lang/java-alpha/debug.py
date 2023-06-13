#!/usr/bin/env python3
# (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

# pyre-strict


import argparse
import glob
import os
import shutil
import tempfile
import time
import zipfile

from xplat.glean.utils import shell_exec


def build_target_with_debug_logs(target: str) -> str:
    cmd = f"buck build -c javac.random_seed={int(time.time())} {target} --show-output --local"
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


def main() -> None:
    parser = argparse.ArgumentParser(fromfile_prefix_chars="@")
    parser.add_argument(
        "target",
        help="Target examples directory to index. Does not need to be the full path, just the directory name. ex: 'generics'",
    )
    parsed_args = parser.parse_args()

    full_target = f"//glean/lang/java-alpha/indexer/examples/java/com/facebook/glean/{parsed_args.target}:{parsed_args.target}-java11"
    print(f"Building target: {full_target}")

    jar_path = build_target_with_debug_logs(full_target)
    print(f"jar with debug logs: {jar_path}")

    exctracted_jar_dir = extract_jar_to_tmp_dir(jar_path)
    print_debug_logs(exctracted_jar_dir)
    shutil.rmtree(exctracted_jar_dir, ignore_errors=True)


if __name__ == "__main__":
    main()
