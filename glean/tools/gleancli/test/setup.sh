# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree.

# shellcheck shell=bash

set -uo pipefail

tmp=$(mktemp -d --tmpdir)

shopt -s expand_aliases
alias glean='"$GLEAN" --db-root $tmp'

# try to silence scuba warnings
export GLOG_minloglevel=10
