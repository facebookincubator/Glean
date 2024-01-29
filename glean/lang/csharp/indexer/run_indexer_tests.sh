#!/bin/bash
# (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

DOTNET=$1
NUGET_CONFIG=$2
PROJECT=$3

export SAMPLE_PROJECT_PATH=$4 && $DOTNET test /p:RestoreConfigFile="$NUGET_CONFIG" "$PROJECT"
