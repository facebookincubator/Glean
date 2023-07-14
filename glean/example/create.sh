#! /bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree.

set -e

# Create an example database and open the shell to view its content.

cd ~/fbsource/fbcode/glean/example

DB_NAME=test/0
ARGS=(--db-root /tmp --schema dir:schema --db "$DB_NAME")

glean delete "${ARGS[@]}" || true
glean create "${ARGS[@]}"
glean write  "${ARGS[@]}" facts.glean
glean derive "${ARGS[@]}" example.Child
glean shell  "${ARGS[@]}"
