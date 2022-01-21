#!/usr/bin/env bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree.

set -e

git clone https://github.com/facebookincubator/hsthrift.git
cd hsthrift
./install_deps.sh --nuke
