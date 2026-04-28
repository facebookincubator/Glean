#!/usr/bin/env fbpython
# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree.


import dotslash

dotslash.export_fbcode_build(
    target="fbcode//glean/lang/scip/indexer/scip_to_glean/cli:cli",
    oncall="code_indexing",
    install_platforms={
        dotslash.InstallPlatform.LINUX_AARCH64: {
            "exec_path": "cli",
        },
        dotslash.InstallPlatform.LINUX_X86_64: {
            "exec_path": "cli",
        },
    },
)
