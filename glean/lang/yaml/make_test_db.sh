#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree.

set -ueo pipefail

BUCK_MODE="${BUCK_MODE:-dev}"

FBSOURCE=$(realpath ~/fbsource)
INDEXER=${FBSOURCE}/$(buck2 build "@mode/${BUCK_MODE}" //glean/lang/yaml/indexers:yaml_indexer --show-json-output | jq -r '.["fbcode//glean/lang/yaml/indexers:yaml_indexer"]')

GLEAN_TIER="${GLEAN_TIER:-glean.write.test}"
REPO_NAME="${USER}.test"
REPO_HASH=$(hg log -l 1 -T '{node}')
HANDLE=$(uuidgen)
OUTPUT_DIR=$(mktemp --directory --dry-run)

INPUT_CSV_LINES=$(wc -l < "${INPUT_CSV}")

echo "BUCK_MODE:    ${BUCK_MODE}"
echo "INDEXER:      ${INDEXER}"
echo "TEST_DIR:     ${TEST_DIR:-}"
echo "INPUT_CSV:    ${INPUT_CSV}, lines: ${INPUT_CSV_LINES}"
echo "GLEAN_TIER:   ${GLEAN_TIER}"
echo "REPO_NAME:    ${REPO_NAME}"
echo "REPO_HASH:    ${REPO_HASH}"
echo "HANDLE:       ${HANDLE}"
echo "OUTPUT_DIR:   ${OUTPUT_DIR}"

glean --service "${GLEAN_TIER}" delete --repo-name "${REPO_NAME}" --repo-hash "${REPO_HASH}" || true;
glean --service "${GLEAN_TIER}" create --repo-name "${REPO_NAME}" --repo-hash "${REPO_HASH}" --handle "${HANDLE}";

time "${INDEXER}" \
    --output-dir "${OUTPUT_DIR}" \
    --collect-sources-from-csvfile "${INPUT_CSV}" \
    "$@";

echo "Generated json:"
ls -lah "${OUTPUT_DIR}"
echo "Uploading json to Glean..."
time find "${OUTPUT_DIR}" -name '*.json' -print -type f -exec glean --service "${GLEAN_TIER}" write --repo-name "${REPO_NAME}" --repo-hash "${REPO_HASH}" -j 50 '{}' \;

glean --service "${GLEAN_TIER}" complete --repo-name "${REPO_NAME}" --repo-hash "${REPO_HASH}";


glean --service "${GLEAN_TIER}" finish --repo-name "${REPO_NAME}" --repo-hash "${REPO_HASH}" --handle "${HANDLE}";
