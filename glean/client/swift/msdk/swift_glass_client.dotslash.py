#!/usr/bin/env fbpython
# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree.

# https://www.internalfb.com/intern/msdk/bump/swift_glass_client

import dotslash

dotslash.export_fbcode_build(
    target="fbcode//glean/client/swift:swift_glass_client",
    install_platforms={
        dotslash.InstallPlatform.LINUX_X86_64,
        dotslash.InstallPlatform.LINUX_AARCH64,
        dotslash.InstallPlatform.MAC,
    },
    oncall="swift_lsp",
)
