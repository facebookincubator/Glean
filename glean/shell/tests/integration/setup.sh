#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree.

mkdir "$TMPDIR/db"

export GLOG_minloglevel=10

"$SERVER" \
    --db-root "$TMPDIR/db" \
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

