#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree.

set -ueo pipefail

export DOTNET_ROOT=arvr/projects/socialvr/third-party/dotnet/linux-x64/

DOTNET_HOST_PATH="$DOTNET_ROOT"/dotnet
REPO_ROOT=$(hg root)
PROJECT="$REPO_ROOT"/fbcode/glean/lang/csharp/discovery/Glean.Discovery.csproj
PROJECT_ROOT=$(dirname "$PROJECT")

function cleanup {
    set -e
    rm -rf "${PROJECT_ROOT:?}"/bin/
    rm -rf "${PROJECT_ROOT:?}"/obj/
}
trap cleanup EXIT

$DOTNET_HOST_PATH run --project "$PROJECT"
