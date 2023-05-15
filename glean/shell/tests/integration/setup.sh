#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree.

mkdir "$TMPDIR/db"

export GLOG_minloglevel=10
export FOLLY_LOG_LEVEL=ERROR

"$SERVER" \
    --db-root "$TMPDIR/db" \
    --logging="$FOLLY_LOG_LEVEL" \
    --write-port "$TMPDIR/port" \
    --schema "$EXAMPLE/schema" &

SERVER_PID=$!
trap "kill -TERM $SERVER_PID" EXIT

while ! test -f "$TMPDIR/port"; do sleep 1; done
PORT=$(cat "$TMPDIR/port")

DB=example/0

"$GLEAN" \
    --service "::1:$PORT" \
    create --repo "$DB" \
    "$EXAMPLE/facts.glean" \
    --finish

