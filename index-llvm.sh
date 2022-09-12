#!/usr/bin/env bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree.

set -e
shopt -s extglob
shopt -s inherit_errexit

fatal() {
  echo "$@" 1>&2
  exit 1
}

usage() {
  cat << EOF
Usage: $(basename "$0") FLAGS SOURCE_DIR TARGET_DIR

  Index LLVM source distribution located in SOURCE_DIR into TARGET_DIR

Example:
  mkdir -p /tmp/llvm \\
    && ( wget -O - https://github.com/llvm/llvm-project/releases/download/llvmorg-14.0.6/llvm-14.0.6.src.tar.xz \\
          | tar -C /tmp/llvm -xJ ) \\
    && $0 -j$(grep -c '^processor' /proc/cpuinfo) /tmp/llvm/llvm-14.0.6.src /tmp/llvm


Available options:
  --glean DIR             Specify Glean source directory
  -jN | --jobs N          Use N concurrent indeces
  --db-root DIR           Store the generated database in DIR
                          (default TARGET_DIR/db)
  --db DB                 Use provided name for the database
                          (default llvm/VERSION)
  --overwrite             Overwrite the database if it already exists
  --schema DIR            Use the schema in DIR (default is the schema in
                          Glean's source tree)
EOF
}

argerror() {
  echo "$@" >&2
  usage >&2
  exit 1
}

if [[ "$1" == "--help" ]]
then
  usage
  exit 0
fi

MAKE_ARGS=()
EXTRA_GLEAN_ARGS=()
EXTRA_GLEAN_INDEX_ARGS=()
VERBOSITY=0
GLEAN_DB_ROOT=
GLEAN_SCHEMA=
GLEAN_DIR=

while true
do
  case "$1" in
    (-j+([0-9]))
      MAKE_ARGS+=("$1")
      EXTRA_GLEAN_INDEX_ARGS+=("$1")
      shift
      ;;
    -j|--jobs)
      MAKE_ARGS+=("$1")
      EXTRA_GLEAN_INDEX_ARGS+=("$1")
      case "$2" in
        (+([0-9]))
          MAKE_ARGS+=("$2")
          EXTRA_GLEAN_INDEX_ARGS+=("$2")
          shift 2
          ;;
        *)
          argerror "Invalid number of jobs"
          ;;
      esac
      ;;
    -v|--verbose)
      VERBOSITY=1
      EXTRA_GLEAN_INDEX_ARGS+=("-v")
      shift
      ;;
    --db-root)
      GLEAN_DB_ROOT="$2"
      shift 2
      ;;
    --schema)
      GLEAN_SCHEMA="$2"
      shift 2
      ;;
    --db)
      GLEAN_DB="$2"
      shift 2
      ;;
    --overwrite)
      OVERWRITE=yes
      shift
      ;;
    --glean)
      GLEAN_DIR="$2"
      shift 2
      ;;
    -*)
      argerror "Unsupported option $1"
      ;;
    *)
      break
      ;;
  esac
done

if [[ -z "$1" ]]
then
  argerror "No source directory specified"
fi

SOURCE_DIR="$1"
shift

if [[ -z "$1" ]]
then
  argerror "No destination directory specified"
fi

OUTPUT_DIR="$1"
shift

if [[ $# -ne 0 ]]
then
  argerror "Extra arguments"
fi

if [[ -z "${GLEAN_DIR}" ]]
then
  # Look for any ancestor directory with glean.cabal.in - this allows us to
  # move the script within the source tree
  dir=$(dirname "$0")
  while [[ "${dir}" != "/" ]] ; do
    if [[ -f "${dir}"/glean.cabal.in ]]
    then
      GLEAN_DIR="${dir}"
      break
    fi
    dir=$(dirname "${dir}")
  done
  if [[ -z "${GLEAN_DIR}" ]] ; then
    fatal "Couldn't local Glean source directory, please specify --glean"
  fi
fi

BUILD_DIR="${OUTPUT_DIR}"/build
mkdir -p "${BUILD_DIR}"
GLEAN_DB_ROOT="${GLEAN_DB_ROOT:-${OUTPUT_DIR}/db}"
mkdir -p "${GLEAN_DB_ROOT}"

GLEAN_SCHEMA="${GLEAN_SCHEMA:-${GLEAN_DIR}/glean/schema/source}"

# FD 5 is where we redirect all output
if [[ "${VERBOSITY}" == "1" ]] ; then
  exec 5>&0
else
  exec 5> "${OUTPUT_DIR}/index-llvm.log"
fi

echo "Building glean-clang"
make MODE=opt glean-clang >&5

requirebin() {
  tmp="$("${GLEAN_DIR}/quick.sh" MODE=opt list-bin "$1")"
  if ! [[ -x "${tmp}" ]] ; then
    fatal "$1 doesn't exist at ${tmp}"
  fi
  echo "${tmp}"
}

CLANG_INDEX=$(requirebin glean-clang:clang-index)
CLANG_DERIVE=$(requirebin glean-clang:clang-derive)
GLEAN=$(requirebin glean:glean)

echo "Setting up ${BUILD_DIR}"
mkdir -p "${BUILD_DIR}" >&5
"${CMAKE:-cmake}" \
  -DCMAKE_EXPORT_COMPILE_COMMANDS=1 \
  -DLLVM_INCLUDE_BENCHMARKS=OFF -DLLVM_TARGETS_TO_BUILD=X86 \
  -S "${SOURCE_DIR}" -B "${BUILD_DIR}" >&5

echo "Generating LLVM code"
# We get all the *TableGen targets from make help which thankfully lists all
# available targets. We do want word splitting here so disable the corresponding
# check.
# shellcheck disable=SC2046
(cd "${BUILD_DIR}" && \
  make "${MAKE_ARGS[@]}" intrinsics_gen acc_gen omp_gen llvm_vcsrevision_h \
    $(make help | sed -n '/^[.][.][.] [A-Za-z0-9]*TableGen$/ s/^....//p')) >&5

if [[ -z "${GLEAN_DB}" ]] ; then
  GLEAN_DB=llvm/$(sed -n '/^Version/ {s/^Version: //p;q}' "${BUILD_DIR}/llvm.spec")
fi

if [[ -n "${OVERWRITE}" ]] ; then
  rm -rf "${GLEAN_DB_ROOT:?}/${GLEAN_DB}"
fi

echo "Indexing ${GLEAN_DB} in ${GLEAN_DB_ROOT}"
"${GLEAN}" "${EXTRA_GLEAN_ARGS[@]}" \
  --schema "${GLEAN_SCHEMA}" --db-root "${GLEAN_DB_ROOT}" \
  index --db "${GLEAN_DB}" cpp-cmake \
  --indexer "${CLANG_INDEX}" --deriver "${CLANG_DERIVE}" --cdb "${BUILD_DIR}" \
  --verbose --progress "${EXTRA_GLEAN_INDEX_ARGS[@]}" "${SOURCE_DIR}" 2>&5
