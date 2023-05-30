#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree.

# MSDK script to create Kotlin glean indexer
#
# In order to start a version bump, visit the MSDK page for `kotlin_glean_indexer` and click the
# "Trigger Land Bump" button.

import json


if __name__ == "__main__":
    job_capabilities = {
        "vcs": "full-fbsource",
        "type": "lego",
    }

    spec = {
        "trigger_spec": {
            "script": {
                "build_script": "fbcode/glean/lang/kotlin/indexer/scripts/build.py",
                "sandcastle_builds": [
                    {
                        "msdk_output_dir": "output",
                        "override_capabilities": job_capabilities,
                    }
                ],
            }
        },
        "bundle_spec": {
            "chunks": [
                {
                    "path_root": "output",
                    "paths": ["indexer.jar"],
                    "tags": {"ComponentName": "indexer", "InstallTarget": "None"},
                }
            ]
        },
        "commit_spec": {
            # The generated .bzl file will contain a struct that can be parsed into
            # http_archive() rules by MSDK infra.
            "sandcastle": {"command": "SandcastleMSDKBumpCommitBzlCommand"},
        },
    }
    print(json.dumps(spec))
