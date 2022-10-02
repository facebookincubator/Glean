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
    build|run|test|list-bin|cxx-test)
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

[ -n "$ACTION" ] || fatal "No action specified"

TARGET=$1
[ -n "$TARGET" ] || fatal "No target specified"
shift

make "${MAKE_ARGS[@]}" .build/current.sh glean.cabal cxx-libraries

. .build/current.sh

case $ACTION in
  cxx-test)
    make "${MAKE_ARGS[@]}" "cxx-test-$TARGET"
    ;;

  *)
    CABAL_ARGS=()
    # Suppress "Up to date" etc. for list-bin
    if [ "$ACTION" = "list-bin" ]; then
      CABAL_ARGS+=(-vsilent)
    fi

    call_cabal "${CABAL_ARGS[@]}" "${ACTION}" "${TARGET}" -- "$@"
    ;;
esac