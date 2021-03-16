#!/usr/bin/env bash

set -e

git clone https://github.com/facebookincubator/hsthrift.git
cd hsthrift
./install_deps.sh --nuke
cd ..
