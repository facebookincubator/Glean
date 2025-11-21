#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree.

# Test script for scip-symbol-parse CLI tool

set -euo pipefail

# Binary path is passed as first argument when run via Buck
# Or we build it ourselves when run directly
if [ $# -ge 1 ]; then
    BINARY="$1"
else
    # Build the binary first (for direct execution)
    BINARY="$(buck2 build //fbcode/glean/lang/scip/indexer/scip_to_glean/scip_symbol_parse:scip-symbol-parse --show-output 2>/dev/null | awk '{print $2}')"
fi

if [ ! -f "$BINARY" ]; then
    echo "Error: Binary not found at: $BINARY"
    exit 1
fi

TESTS_PASSED=0
TESTS_FAILED=0

# Helper function to run a test
run_test() {
    local test_name="$1"
    local input="$2"
    local expected_output="$3"
    local use_stdin="${4:-false}"

    local actual_output
    if [ "$use_stdin" = "true" ]; then
        actual_output=$(echo "$input" | "$BINARY" --stdin 2>&1)
    else
        actual_output=$("$BINARY" "$input" 2>&1)
    fi

    if [ "$actual_output" = "$expected_output" ]; then
        echo "✓ PASSED: $test_name"
        TESTS_PASSED=$((TESTS_PASSED + 1))
        return 0
    else
        echo "❌ FAILED: $test_name"
        echo "Expected:"
        echo "$expected_output"
        echo ""
        echo "Got:"
        echo "$actual_output"
        echo ""
        TESTS_FAILED=$((TESTS_FAILED + 1))
        return 1
    fi
}

echo "Running scip-symbol-parse tests..."
echo ""

run_test "Local symbol" \
    "local myVar" \
    '{"type":"local","id":"myVar"}'

run_test "Global symbol with descriptors" \
    "rust-analyzer cargo std v1.0 io/IsTerminal#" \
    '{"type":"global","scheme":"rust-analyzer","package":{"manager":"cargo","name":"std","version":"v1.0"},"descriptors":[{"name":"io","kind":{"kind":"namespace"}},{"name":"IsTerminal","kind":{"kind":"type"}}]}'

run_test "Method with disambiguator" \
    "scip . . . MyClass#myMethod(sig)." \
    '{"type":"global","scheme":"scip","package":{"manager":null,"name":null,"version":null},"descriptors":[{"name":"MyClass","kind":{"kind":"type"}},{"name":"myMethod","kind":{"kind":"method","disambiguator":"sig"}}]}'

run_test "stdin mode" \
    "local foo
local bar" \
    '{"type":"local","id":"foo"}
{"type":"local","id":"bar"}' \
    true

run_test "Escaped identifier with space" \
    "scip . . . \`has space\`#" \
    '{"type":"global","scheme":"scip","package":{"manager":null,"name":null,"version":null},"descriptors":[{"name":"has space","kind":{"kind":"type"}}]}'

run_test "Escaped identifier with backticks" \
    "scip . . . \`name\`\`with\`\`ticks\`#" \
    '{"type":"global","scheme":"scip","package":{"manager":null,"name":null,"version":null},"descriptors":[{"name":"name`with`ticks","kind":{"kind":"type"}}]}'

run_test "Space-escaped fields in package" \
    "my  scheme my  manager my  package my  version MyClass#" \
    '{"type":"global","scheme":"my scheme","package":{"manager":"my manager","name":"my package","version":"my version"},"descriptors":[{"name":"MyClass","kind":{"kind":"type"}}]}'

run_test "Rubbish input" \
    "qwerwekr" \
    '{"type":"global","scheme":"qwerwekr","package":{"manager":"","name":"","version":""},"descriptors":[]}'

echo ""
echo "================================"
echo "Test Results:"
echo "  Passed: $TESTS_PASSED"
echo "  Failed: $TESTS_FAILED"
echo "================================"

if [ $TESTS_FAILED -eq 0 ]; then
    echo "✅ All tests passed!"
    exit 0
else
    echo "❌ Some tests failed"
    exit 1
fi
