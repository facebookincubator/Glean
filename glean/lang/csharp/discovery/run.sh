#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree.

set -ueo pipefail

title() {
  echo -e "\033[0;34m""$1""\033[0m" >&2
}

warning() {
  echo -e "\033[0;33m""$1""\033[0m" >&2
}

enable_discovery_buck=default
enable_discovery_msbuild=default
enable_discovery_unity=default

limit=none
deterministic=false
skip=0
log_level=Information

while [[ "$#" -gt 0 ]]; do
  case $1 in
    --enable-discovery-buck)
    enable_discovery_buck="$2"
    shift
    ;;
    --enable-discovery-msbuild)
    enable_discovery_msbuild="$2"
    shift
    ;;
    --enable-discovery-unity)
    enable_discovery_unity="$2"
    shift
    ;;
    --log-level)
    log_level="$2"
    shift
    ;;
    --limit)
    limit="$2"
    shift
    ;;
    --skip)
    skip="$2"
    shift
    ;;
    --deterministic)
    deterministic=true
    ;;
  esac
  shift
done

if [ "$enable_discovery_buck" = "default" ]; then
  if jk check code_indexing/csharp:enable_discovery_buck --exit-with-code; then
    enable_discovery_buck="true"
  else
    enable_discovery_buck="false"
  fi
fi

if [ "$enable_discovery_msbuild" = "default" ]; then
  if jk check code_indexing/csharp:enable_discovery_msbuild --exit-with-code; then
    enable_discovery_msbuild="true"
  else
    enable_discovery_msbuild="false"
  fi
fi

if [ "$enable_discovery_unity" = "default" ]; then
  if jk check code_indexing/csharp:enable_discovery_unity --exit-with-code; then
    enable_discovery_unity="true"
  else
    enable_discovery_unity="false"
  fi
fi

repo_root=$(hg root)
out=$(mktemp -d)

export dotnet_root="$repo_root"/arvr/projects/socialvr/third-party/dotnet/linux-x64
dotnet_host_path="$dotnet_root"/dotnet

discovery_project_root="$repo_root"/fbcode/glean/lang/csharp/discovery
discovery_project="$discovery_project_root"/Glean.Discovery.csproj

indexer_project_root="$repo_root"/fbcode/glean/lang/csharp/indexer
indexer_project="$indexer_project_root"/Glean.Indexer.csproj

changeset=$(hg id)

if [[ "$changeset" == *"+" ]]; then
  warning "Warning: you have uncommitted changes. Do you want to run 'hg amend -i'? (y/n)"
  read -r answer
  if [[ "$answer" == "y" ]]; then
    echo "">&2
    hg amend -i
    changeset=$(hg id)
  fi
  echo "">&2
fi

paths=/tmp/"$changeset"/paths
unity_work="$out"/unity_work.json
msbuild_work="$out"/msbuild_work.json
all_work="$out"/all_work.json
materialized_work="$out"/materialized_work.json
facts="$out"/facts

function cleanup {
    set -e
    rm -rf "${discovery_project_root:?}"/bin
    rm -rf "${discovery_project_root:?}"/obj
    rm -rf "${indexer_project_root:?}"/bin
    rm -rf "${indexer_project_root:?}"/obj
    if [[ "$changeset" == *"+" ]]; then
      rm "$paths"
    fi
}
trap cleanup EXIT

if [ "$enable_discovery_msbuild" = "true" ]; then
  if [ ! -f "$paths" ]; then
    mkdir -p "$(dirname "$paths")"
    title "Enumerating MSBuild projects/solutions"
    eden glob arvr/**/*.{csproj,sln} --repo "$repo_root" --list-only-files > "$paths"
    echo Wrote list of projects/solutions to "$paths""
    ">&2
  else
    warning "Skipping enumerating MSBuild projects/solutions, reusing results from $paths
    ">&2
  fi

  title "Discovering MSBuild work"
  $dotnet_host_path run --project "$discovery_project" -- discover msbuild "$repo_root" "$msbuild_work" < "$paths" >&2
  echo "">&2
fi

if [ "$enable_discovery_unity" = "true" ]; then
  title "Discovering Unity work"
  $dotnet_host_path run --project "$discovery_project" -- discover unity "$repo_root" "$unity_work" >&2
  echo "">&2
fi

work_files=()

if [ "$enable_discovery_msbuild" = "true" ]; then
  work_files+=("$msbuild_work")
fi

if [ "$enable_discovery_unity" = "true" ]; then
  work_files+=("$unity_work")
fi

title "Collating all discovered work"
if [ ${#work_files[@]} -ne 0 ]; then
  if [ "$deterministic" = true ]; then
    jq -s add "${work_files[@]}" | jq ".[$skip:]" > "$all_work"
  else
    jq -s add "${work_files[@]}" | jq -c '.[]' | shuf | jq -s . > "$all_work"
  fi
else
  echo '[]' > "$all_work"
fi
echo Wrote collated work to "$all_work""
">&2

if [ "$limit" != "none" ]; then
  temp=$(jq ".[0:$limit]" < "$all_work")
  echo "$temp" > "$all_work"
fi

template="$repo_root"/arvr/projects/socialvr/tools/zero_unity_devtools/ZeroUnityDevTools/HorizonCsprojViaApplyTemplate.csproj_template

title "Materializing work"
$dotnet_host_path run --project "$discovery_project" -- materialize "$template" "$materialized_work" < "$all_work" >&2
echo "" >&2

if [ "$limit" != "none" ]; then
  temp=$(jq ".[0:$limit]" < "$materialized_work")
  echo "$temp" > "$materialized_work"
fi

title "Indexing work"
$dotnet_host_path run --project "$indexer_project" -- "$materialized_work" --out "$facts" --log-level "$log_level" >&2

db_root="$out"/db
db_name=test/0

args=(--db-root "$db_root" --schema "$repo_root"/fbcode/glean/schema/source --db "$db_name")
echo "$facts"/*.json | xargs glean create "${args[@]}"

glean derive "${args[@]}" csharp.FileDefinitions
glean derive "${args[@]}" csharp.FileEntityXRefs
glean derive "${args[@]}" csharp.NameLowerCase

glean shell "${args[@]}"
