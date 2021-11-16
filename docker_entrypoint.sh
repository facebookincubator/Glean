#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree.

set -e

if [ "$1" = 'shell' ]; then
# short cutting demo and going straight to shell
    shift
    exec bash "$@"
    exit 0
elif mount | grep '/glean-demo/code' >/dev/null ; then
# test for mounted code and try to index it
    if [ ! -f code/.flowconfig ]; then
        echo "Your project must have a .flowconfig"
        exit 2
    fi
    REPO_NAME="${REPO_NAME#react}"
    REPO_NAME="${REPO_NAME:-code}"
    rm -Rf db/${REPO_NAME}
    mkdir -p /tmp/flow-index-out
    flow glean code --output-dir /tmp/flow-index-out --write-root ""
    glean --db-root db --db-schema dir:schema/source create --repo ${REPO_NAME}/0
    glean --db-root db --db-schema dir:schema/source write --repo ${REPO_NAME}/0 /tmp/flow-index-out/*
    glean --db-root db --db-schema dir:schema/source derive --repo ${REPO_NAME}/0 flow.FileXRef flow.FileDeclaration
    glean --db-root db --db-schema dir:schema/source finish --repo ${REPO_NAME}/0
    rm -Rf /tmp/flow-index-out
fi

echo "Starting Glean Server"
exec bin/glean-server --db-root db --schema schema/source --port 12345 &
sleep 2
echo "Starting Hyperlink Server"
exec bin/glean-hyperlink --service localhost:12345 --repo ${REPO_NAME} --root code --http 8888
echo "Exiting.."
