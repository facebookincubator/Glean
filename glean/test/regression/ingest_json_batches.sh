#!/usr/bin/env bash

# This script will
#   1. Run an indexer over source files
#   2. Create a Glead DB from the indexer results
#   3. Derive predicates in that DB
#
# It expects the following arguments
#   1. Path to Glean CLI executable
#   2. Path to a test Glean DB root
#   3. Path to schema files directory
#   4. Name of glean repo to be used
#   5. Comma separated (no spaces) list of predicates to be derived
#   6. Path of indexer executable
#   7..n. Arguments to be passed to the indexer. These may contain
#       $JSON_BATCH_DIR, which will be replaced with a path where
#       the JSON results of indexing should be saved.

set -euo pipefail

GLEAN_CLI="$1"
DB_ROOT="$2"
SCHEMA="$3"
REPO="$4"
DERIVED="$5"
shift 5

JSON_BATCH_DIR=$(mktemp -d)
trap "rm -rf \"$JSON_BATCH_DIR\"" EXIT
# Prepare indexing command.
# Replace $JSON_BATCH_DIR with a path to a temporary directory.
# `cmd` will contain: [<indexer-binary>, arg1, arg2 ...]
declare -a cmd
for arg in "$@"; do
    cmd+=("${arg//\$\{JSON_BATCH_DIR\}/$JSON_BATCH_DIR}")
done

# Index source files
echo "Running: " "${cmd[@]}" >&2
command "${cmd[@]}"

# Add generated predicates to test database
for f in "$JSON_BATCH_DIR"/*; do
    "$GLEAN_CLI" \
        --db-root="$DB_ROOT" \
        --schema="$SCHEMA" \
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
            --schema="$SCHEMA" \
            derive \
            --repo="$REPO" \
            "$p"
    done
fi
