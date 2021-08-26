#!/usr/bin/env bash

# Copyright (c) Facebook, Inc. and its affiliates.

set -e

git clone https://github.com/facebookincubator/hsthrift.git
cd hsthrift
./install_deps.sh --nuke
