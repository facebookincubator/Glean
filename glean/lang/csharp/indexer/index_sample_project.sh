#!/bin/bash
# (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

set -ueo pipefail

sample_project_target=arvr/projects/glean/lang/csharp/Sample:sample_generated_project
indexer_target=arvr/projects/glean/lang/csharp/Indexer:indexer_portable_binary
temp_dir=$(mktemp -d)
indexer_output="$temp_dir"/facts
projects_file="$temp_dir"/projects.json
db_root="$temp_dir"/db
db_name=test/0
facts_limit=15

function cleanup {
    set -e
    rm -rf "$temp_dir"
}
trap cleanup EXIT

mkdir -p "$indexer_output"
mkdir -p "$db_root"
buck build $sample_project_target --show-full-json-output | jq -r '[.[]]' > "$projects_file"
buck run $indexer_target -- "$projects_file" -o "$indexer_output" -l $facts_limit

args=(--db-root "$db_root" --schema ~/fbsource/fbcode/glean/schema/source --db "$db_name")
echo "$indexer_output"/*.json | xargs glean create "${args[@]}"

glean derive "${args[@]}" csharp.FileDefinitions
glean derive "${args[@]}" csharp.FileEntityXRefs
glean derive "${args[@]}" csharp.NameLowerCase

glean shell "${args[@]}"
