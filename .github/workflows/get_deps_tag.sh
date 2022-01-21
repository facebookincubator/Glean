#!/usr/bin/env sh
# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree.

git clone https://github.com/facebook/wangle.git tmp > /dev/null 2>&1
cd tmp && git tag | sort -r | head -1 && cd ..
rm -rf tmp
