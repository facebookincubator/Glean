#! /bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree.

set -e

fatal() {
  echo "$@" 1>&2
  exit 1
}

MAKE_ARGS=()

for arg in "$@"; do
  case $arg in
    build|run|test)
      ACTION="$1"
      shift
      break
      ;;
    *)
      MAKE_ARGS+=("${arg}")
      shift
      ;;
  esac
done

TARGET=$1
shift

if [ -z "$ACTION" ]; then
  fatal "No action specified"
fi
if [ -z "$TARGET" ]; then
  fatal "No target specified"
fi

make "${MAKE_ARGS[@]}" .build/current.sh glean.cabal cxx-libraries

. .build/current.sh

call_cabal "${ACTION}" "${TARGET}" -- "$@"
