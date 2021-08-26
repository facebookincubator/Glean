#!/bin/bash

# Copyright (c) Facebook, Inc. and its affiliates.

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

