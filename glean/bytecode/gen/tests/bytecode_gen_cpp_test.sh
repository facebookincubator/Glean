#! /bin/bash -e
# (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

# Precondition:
#    run with current workding directory of fbsource/fbcode
#
# Arguments
#    one parameter which is the bytecode-gen command to run, which is passed
#       --install-dir=<root_directory_for_bytecode_gen>

dir="$(mktemp -d /tmp/glean_bytecode_gen_cpp_test_XXXXXX)"

function cleanup() { rm -rf "${dir}"; }
trap cleanup EXIT

function help() {
    cat 1>&2 <<EOF

***********************************************************
If this test failed, it is probably because you need to run
  glean/bytecode/sync
***********************************************************

EOF
}
trap help ERR

# Get absolute filepath to this script, and then its directories
this_file=$(readlink -f "${0}")
bytecode_dir=$(dirname $(dirname $(dirname "${this_file}")))

# Execute the command and pass it the temp dir
"$1" --install_dir="${dir}"

# Check that the thrift files in glean/schema match the new ones in temp dir
for i in "${dir}"/*; do
    diff -c "$i" "${bytecode_dir}/${i#${dir}}"
done
