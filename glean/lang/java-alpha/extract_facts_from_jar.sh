#!/usr/bin/env bash
# (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

set -ueo pipefail
set -x

INDEXER_JEX="${1:?First argument needs to be an indexer .jex file}"
TESTCASE="${2:?Second argument needs to be a test case folder}"
DEST="${3:?Third argument needs to be a directory to extract fact jsons to}"
JAVAC="${4:?Fourth argument needs to be the javac binary}"

TMP=$(mktemp -d)
trap "rm -rf ""$TMP" EXIT

unzip "$INDEXER_JEX" java_alpha.jar -d "$TMP" || true
INDEXER="${TMP}/java_alpha.jar"

mkdir -p "$DEST" &>/dev/null || true
cd "$DEST"

"$JAVAC" -Xplugin:Indexer -processorpath "$INDEXER" -classpath "$TESTCASE" @<(find "$TESTCASE" -name '*.java')
