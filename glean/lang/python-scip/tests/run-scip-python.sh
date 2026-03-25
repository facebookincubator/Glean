#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree.

# Wrapper to run @sourcegraph/scip-python via Node.js.
#
# When invoked via buck (exe_target), BUCK_DEFAULT_RUNTIME_RESOURCES points to
# the resource tree which includes the yarn_install output containing
# the scip-python package and its node_modules.
#
# scip-python shells out to `pip` to discover site-packages. On Meta
# devservers `pip list` is broken by a stale setuptools `.pth` file;
# we work around this with a temporary pip shim that handles only
# `pip list` and `pip show` (the two sub-commands scip-python uses).
# On OSS / CI where pip works normally this is harmless: the shim's
# `pip list` still succeeds (it just returns an empty list, which
# scip-python treats the same as "no site-packages to scan").

set -euo pipefail

# --- Locate resources ---
if [[ -n "${BUCK_DEFAULT_RUNTIME_RESOURCES:-}" ]]; then
    RESOURCE_ROOT="$BUCK_DEFAULT_RUNTIME_RESOURCES"
else
    RESOURCE_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
fi

# The yarn_install target produces its output under the full fbcode package
# path within the resource tree.
SCIP_PYTHON_INDEX="$RESOURCE_ROOT/glean/lang/python-scip/tests/scip-python-deps/node_modules/@sourcegraph/scip-python/index.js"

if [[ ! -f "$SCIP_PYTHON_INDEX" ]]; then
    # Fallback: search for it (handles different resource layouts)
    SCIP_PYTHON_INDEX="$(find "$RESOURCE_ROOT" -path "*/scip-python-deps/node_modules/@sourcegraph/scip-python/index.js" -print -quit 2>/dev/null)"
    if [[ -z "$SCIP_PYTHON_INDEX" || ! -f "$SCIP_PYTHON_INDEX" ]]; then
        echo "ERROR: Cannot find scip-python index.js under $RESOURCE_ROOT" >&2
        exit 1
    fi
fi

# --- Pip shim (workaround for broken pip on Meta devservers) ---
PIP_SHIM_DIR=""
cleanup() {
    if [[ -n "$PIP_SHIM_DIR" ]]; then
        rm -rf "$PIP_SHIM_DIR"
    fi
}
trap cleanup EXIT

PIP_SHIM_DIR="$(mktemp -d)"
cat > "$PIP_SHIM_DIR/pip" <<'SHIM'
#!/bin/bash
# Minimal pip shim: scip-python calls `pip list --format=json` and `pip show`.
# Return empty JSON array for `list` so scip-python skips site-packages scanning.
case "${1:-}" in
    list)  echo "[]" ;;
    show)  exit 1 ;;
    *)     exit 0 ;;
esac
SHIM
chmod +x "$PIP_SHIM_DIR/pip"
# scip-python checks for pip3 before pip, so provide both shims
cp "$PIP_SHIM_DIR/pip" "$PIP_SHIM_DIR/pip3"

# Prepend shim dir so it shadows the broken system pip/pip3
export PATH="$PIP_SHIM_DIR:$PATH"

# --- Run scip-python ---
NODE="$(command -v node 2>/dev/null || true)"
if [[ -z "$NODE" ]]; then
    echo "ERROR: node not found on PATH" >&2
    exit 1
fi

exec "$NODE" "$SCIP_PYTHON_INDEX" "$@"
