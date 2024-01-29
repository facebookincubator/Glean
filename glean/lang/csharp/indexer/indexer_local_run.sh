#!/bin/bash
# (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

set -ueo pipefail

determinator="$(hg root)"/xplat/js/tools/glean/csharp-target-determinator/csharp_target_determinator_local_run.sh
indexer_target=//arvr/projects/glean/lang/csharp/Indexer:indexer_portable_binary
temp_dir=$(mktemp -d)
indexer_output="$temp_dir"/facts
projects_file="$temp_dir"/projects.json
db_root="$temp_dir"/db
db_name=test/0
determinator_log_file="$temp_dir"/debug.log
determinator_targets_file="$temp_dir"/targets
generated_unity_projects_directory="$temp_dir"/generated/unity
limit=0

for arg in "$@"
do
    case $arg in
        --limit)
        limit=10
        shift
        ;;
    esac
done

function cleanup {
    set -e
    rm -rf "$temp_dir"
}
trap cleanup EXIT

mkdir -p "$indexer_output"
mkdir -p "$db_root"

$determinator --projects-file "$projects_file" --log-file "$determinator_log_file" --targets-file "$determinator_targets_file" --generated-unity-projects-directory "$generated_unity_projects_directory"

if [ $limit -ne 0 ]; then
    jq ".[:$limit]" "$projects_file" > "$projects_file".tmp && mv "$projects_file".tmp "$projects_file"
fi

buck run $indexer_target -- "$projects_file" -o "$indexer_output"

args=(--db-root "$db_root" --schema ~/fbsource/fbcode/glean/schema/source --db "$db_name")
echo "$indexer_output"/*.json | xargs glean create "${args[@]}"

glean shell "${args[@]}"
