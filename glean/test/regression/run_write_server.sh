#!/usr/bin/env bash

set -euo pipefail
GLEAN_SERVER="$1"
DB_ROOT="$2"
SCHEMA="$3"
shift 3

env ASAN_OPTIONS="detect_leaks=0" "$GLEAN_SERVER" -p 0 --db-root "$DB_ROOT" --handler glean --schema="$SCHEMA" &
SERVER_PID="$!"
trap "kill $SERVER_PID" EXIT
TRIES=0
OPEN_PORTS=""
while [ "$TRIES" -lt 60 ] && [ "$OPEN_PORTS" = "" ] && jobs %1 &>/dev/null; do
    sleep 0.5
    OPEN_PORTS=$(lsof -w -iTCP -a -sTCP:LISTEN -a -p "$SERVER_PID" -Fn || true)
    (( TRIES=TRIES+1 ))
done
SERVER_PORT=$(echo "$OPEN_PORTS" | grep -o ':[0-9]\+$' | tr -d ':')

declare -a cmd
for arg in "$@"; do
    cmd+=("${arg//\$\{SERVER_PORT\}/$SERVER_PORT}")
done

echo "Running: " "${cmd[@]}" >&2
command "${cmd[@]}"

kill "$SERVER_PID"
trap - EXIT
wait
