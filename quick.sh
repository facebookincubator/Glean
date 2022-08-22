#! /bin/bash

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

call_cabal $ACTION $TARGET -- "$@"
