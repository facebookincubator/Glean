#!/usr/bin/env bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree.

#
# Note, if you set GLEAN_DEPS_WITH_SUDO=1 hsthrift deps will be installed into
# /usr/local, which requires sudo privs, and you'll need to set:
#
# > export LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH
# > export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig
#
# You will need --sudo passed to ./install_deps.sh
#
# To build without root privs (the default), you will need to set:
#
# > export LD_LIBRARY_PATH=$HOME/.hsthrift/lib
# > export PKG_CONFIG_PATH=$HOME/.hsthrift/lib/pkgconfig
# > export PATH=$PATH:$HOME/.hsthrift/bin
#

set -e

HSTHRIFT_REPO=https://github.com/facebookincubator/hsthrift.git

if test ! -d hsthrift; then
    git clone "${HSTHRIFT_REPO}"
fi

cd hsthrift
if test -z "${GLEAN_DEPS_WITH_SUDO}"; then
    ./new_install_deps.sh
else
    ./install_deps.sh --nuke
fi
