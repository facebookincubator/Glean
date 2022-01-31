#!/usr/bin/env bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree.

#
# To build without root privs (the default), you will need to set:
#
# > export LD_LIBRARY_PATH=$HOME/.hsthrift/lib
# > export PKG_CONFIG_PATH=$HOME/.hsthrift/lib/pkgconfig
# > export PATH=$PATH:$HOME/.hsthrift/bin
#

set -e

HSTHRIFT_REPO=https://github.com/facebookincubator/hsthrift.git
THREADS=4

case "$1" in
    --threads) THREADS="$2"; shift; shift;;
esac

if test ! -d hsthrift; then
    git clone "${HSTHRIFT_REPO}"
fi

cd hsthrift
./new_install_deps.sh rocksdb --threads "${THREADS}"
