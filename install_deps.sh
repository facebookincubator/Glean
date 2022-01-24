#!/usr/bin/env bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree.

#
# Note, if you don't use BUILD_DEPS=1 hsthrift deps will be installed into
# /usr/local, which requires sudo privs, and you'll need to set:
#
# > export LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH
# > export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig#
#

set -e

HSTHRIFT_REPO=https://github.com/facebookincubator/hsthrift.git

if test ! -d hsthrift; then
    git clone "${HSTHRIFT_REPO}"
fi

cd hsthrift
if test -n "${BUILD_DEPS}" && test "${BUILD_DEPS}" -eq 1; then
    ./build.sh build --allow-system-packages --only-deps hsthrift
else
    ./install_deps.sh --nuke --sudo
fi
