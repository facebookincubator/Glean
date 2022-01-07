#!/usr/bin/env bash
# Copyright (c) Facebook, Inc. and its affiliates.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree.

set -e

# Make sure we have updated cabal index before we do anything
cabal update

git clone https://github.com/facebookincubator/hsthrift.git
cd hsthrift
./install_deps.sh --nuke
