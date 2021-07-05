#!/bin/bash

# Copyright (c) Facebook, Inc. and its affiliates.

mkdir "$TMPDIR/db"

"$SERVER" \
    --db-root "$TMPDIR/db" \
    --write-port "$TMPDIR/port" \
    --minloglevel=2 \
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

export GLOG_minloglevel=10
