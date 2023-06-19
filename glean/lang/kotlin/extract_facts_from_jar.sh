#!/usr/bin/env bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree.

set -ueo pipefail
set -x

INDEXER_JEX="${1:?First argument needs to be an indexer .jex file}"
TESTCASE="${2:?Second argument needs to be a test case folder}"
DEST="${3:?Third argument needs to be a directory to extract fact jsons to}"
KOTLINC="${4:?Fourth argument needs to be the javac binary}"
KOTLIN_STDLIB="${5:?Fivth argument needs to be kotlin stdlib}"

mkdir -p "$DEST" &>/dev/null || true
cd "$DEST"

echo "$KOTLINC"

$KOTLINC -Xplugin="$INDEXER_JEX" -d "$DEST" -classpath "$KOTLIN_STDLIB" "$TESTCASE" @<(find "$TESTCASE" -name '*.kt')

rm -rf com
cp ./META-INF/glean-index.json .
rm -rf META-INF
