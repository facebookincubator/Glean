# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree.

#
# opt mode - optimised builds + benchmarks
#

CABAL_CONFIG_FLAGS = --builddir=$(PWD)/$(MODE_DIR)/dist-newbuild --flags=opt --flags=benchmarks
CXX_MODE=make
export CXXFLAGS += -O3 -DNDEBUG
