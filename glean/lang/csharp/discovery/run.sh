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

# The current changeset ID
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

# Absolute path of the current repository's root directory
repo_root=$(hg root)

# A unique temporary directory into which all output is written (excluding C# build outputs)
out=$(mktemp -d)

enable_discovery_buck=default
enable_discovery_msbuild=default
enable_discovery_unity=default

# The maximum number of (materialized) work-items that will be indexed
limit=none

# If deterministic is false, the list of discovered work will be shuffled before being indexed
deterministic=false

# The number of work-items to drop from (the start of) the list of discovered work
skip=0

# Minimum level of events logged by the indexer
log_level=Information

# A file containing an array of repo-root relative paths of MSBuild project/solution files, encoded as JSON
paths=/tmp/"$changeset"/paths

# A file containing an array of Unity work-items, encoded as JSON
unity_work="$out"/unity_work.json

# A file containing an array of MSBuild work-items, encoded as JSON
msbuild_work="$out"/msbuild_work.json

# A file containing an array of all discovered work-items (both MSBuild and Unity), encoded as JSON
all_work="$out"/all_work.json

# A file containing an array materialized work-items, encoded as JSON
materialized_work="$out"/materialized_work.json

# The directory into which the indexer will write fact files
facts="$out"/facts

# Absolute path of the directory containing the .NET CLI
export dotnet_root="$repo_root"/arvr/projects/socialvr/third-party/dotnet/linux-x64

# Absolute path of the .NET CLI
dotnet_host_path="$dotnet_root"/dotnet

discovery_project_root="$repo_root"/fbcode/glean/lang/csharp/discovery
discovery_project="$discovery_project_root"/Glean.Discovery.csproj
indexer_project_root="$repo_root"/fbcode/glean/lang/csharp/indexer
indexer_project="$indexer_project_root"/Glean.Indexer.csproj

# Absolute path of a template file used to generate Unity projects
template="$repo_root"/arvr/projects/socialvr/tools/zero_unity_devtools/ZeroUnityDevTools/HorizonCsprojViaApplyTemplate.csproj_template

db_root="$out"/db
db_name=test/0

glean_args=(--db-root "$db_root" --schema "$repo_root"/fbcode/glean/schema/source --db "$db_name")

# An array of files containing work-items that will be collated before being fed to the indexer
work_files=()

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

title "Materializing work"
$dotnet_host_path run --project "$discovery_project" -- materialize "$template" "$materialized_work" < "$all_work" >&2
echo "" >&2

if [ "$limit" != "none" ]; then
  temp=$(jq ".[0:$limit]" < "$materialized_work")
  echo "$temp" > "$materialized_work"
fi

title "Indexing work"
$dotnet_host_path run --project "$indexer_project" -- "$materialized_work" --out "$facts" --log-level "$log_level" >&2

echo "$facts"/*.json | xargs glean create "${glean_args[@]}"

glean derive "${glean_args[@]}" csharp.FileDefinitions
glean derive "${glean_args[@]}" csharp.FileEntityXRefs
glean derive "${glean_args[@]}" csharp.NameLowerCase
glean derive "${glean_args[@]}" csharp.UnityPackageToProject
glean derive "${glean_args[@]}" csharp.SourceFileToProject
glean derive "${glean_args[@]}" csharp.ProjectToSolution

glean shell "${glean_args[@]}"
