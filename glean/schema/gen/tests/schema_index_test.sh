#! /bin/bash -e
# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree.

dir="$(mktemp -d /tmp/glean_schema_index_test_XXXXXX)"

function cleanup() { rm -rf "${dir}"; }
trap cleanup EXIT

gen_schema="$1"
schema_source="$2"

# check that we can create an index
"$gen_schema" --dir "$schema_source" --update-index "$dir"/index 2>&1 | \
    grep "New index contains 1 instance"

test -f "$dir"/index
test -d "$dir"/instance

# check for idempotence
"$gen_schema" --dir "$schema_source" --update-index "$dir"/index 2>&1 | \
    grep "New index contains 1 instance"

test -f "$dir"/index
test -d "$dir"/instance

# add a new schema
file="$dir"/schema
"$gen_schema" --dir "$schema_source" --source "$file"
echo "schema all.9999 {}" >> "$file"
"$gen_schema" --input "$file" --update-index "$dir"/index 2>&1 | \
    grep "New index contains 2 instance(s)"

# add back the original one
"$gen_schema" --dir "$schema_source" --update-index "$dir"/index 2>&1 | \
    grep "New index contains 2 instance(s)"
