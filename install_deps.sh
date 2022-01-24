#!/usr/bin/env bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree.

set -e

HSTHRIFT_REPO=https://github.com/donsbot/hsthrift.git

if test ! -d hsthrift; then
    git clone "${HSTHRIFT_REPO}"
fi

cd hsthrift
if test -n "${BUILD_DEPS}" && test "${BUILD_DEPS}" -eq 1; then
    ./build.sh build --allow-system-packages --only-deps hsthrift
else
    ./install_deps.sh --nuke --sudo
fi
