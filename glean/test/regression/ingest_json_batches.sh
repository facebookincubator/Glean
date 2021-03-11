#!/usr/bin/env bash

set -euo pipefail

GLEAN_CLI="$1"
DB_ROOT="$2"
SCHEMA="$3"
REPO="$4"
DERIVED="$5"
shift 5

JSON_BATCH_DIR=$(mktemp -d)
trap "rm -rf \"$JSON_BATCH_DIR\"" EXIT
declare -a cmd
for arg in "$@"; do
    cmd+=("${arg//\$\{JSON_BATCH_DIR\}/$JSON_BATCH_DIR}")
done

echo "Running: " "${cmd[@]}" >&2
command "${cmd[@]}"
for f in "$JSON_BATCH_DIR"/*; do
    "$GLEAN_CLI" \
        --db-root="$DB_ROOT" \
        --db-schema="$SCHEMA" \
        write \
        --repo="$REPO" \
        "$f"
done

# Derive predicates if any have been specified
if [ -n "$DERIVED" ]
then
    IFS=',' read -r -a preds <<< "$DERIVED"
    for p in "${preds[@]}"
    do
        echo "Deriving $p"
        "$GLEAN_CLI" \
            --db-root="$DB_ROOT" \
            --db-schema="$SCHEMA" \
            derive \
            --repo="$REPO" \
            "$p"
    done
fi
