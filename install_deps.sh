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
EXTRA_DEPS="rocksdb"

while [ "$#" -gt 0 ]; do
    case "$1" in
        --threads) THREADS="$2"; shift;;
        --use-system-libs) EXTRA_DEPS="";;
        *)
            echo "syntax: install_deps.sh [--use-system-libs] [--threads N]"
            exit 1;;
    esac
    shift
done

if test ! -d hsthrift; then
    git clone "${HSTHRIFT_REPO}"
fi

# Make sure this is available for a subsequent cabal update
if test ! -f glean.cabal; then
    make glean.cabal
fi

cd hsthrift
./new_install_deps.sh --no-fbthrift "${EXTRA_DEPS}" --threads "${THREADS}"
