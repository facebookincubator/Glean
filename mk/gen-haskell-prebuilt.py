#!/usr/bin/env python3
"""
Generate haskell_prebuilt_library() rules for Cabal dependencies.

Uses 'ghc-pkg field --ipid' to resolve each package by its exact unit ID
(with resolved absolute paths), then creates:

  third-party/haskell/
    db/       GHC package database with absolute-path conf files
    libs/     Symlinks to .a files
    BUCK      haskell_prebuilt_library() rules

Run from the Glean repository root.
"""

import os
import re
import subprocess
import shutil
import sys

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------

GHC_VERSION = "9.4.8"
GHC_PKG = os.path.expanduser(f"~/.ghcup/ghc/{GHC_VERSION}/bin/ghc-pkg")
GLEAN_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

GLOBAL_DB = os.path.expanduser(
    f"~/.ghcup/ghc/{GHC_VERSION}/lib/ghc-{GHC_VERSION}/lib/package.conf.d"
)
STORE_DB = os.path.expanduser(f"~/.cabal/store/ghc-{GHC_VERSION}/package.db")
INPLACE_DB = os.path.join(
    GLEAN_ROOT, f"dist-newstyle/packagedb/ghc-{GHC_VERSION}"
)
ALL_DBS = [GLOBAL_DB, STORE_DB, INPLACE_DB]

TARGET_DIR = os.path.join(GLEAN_ROOT, "third-party/haskell")
TARGET_DB  = os.path.join(TARGET_DIR, "db")
TARGET_LIBS = os.path.join(TARGET_DIR, "libs")

# Packages built as buck2 haskell_library targets (not prebuilt).
# Maps package name -> buck2 target label.
BUCK2_PACKAGES = {
    "mangle": "//hsthrift/common/mangle:mangle",
}

# Packages to skip entirely.
SKIP_PACKAGES = {"folly-clib", "fb-stubs"}

# Fields we request from ghc-pkg for building conf files and BUCK rules.
INFO_FIELDS = (
    "name,version,id,"
    "exposed-modules,hidden-modules,"
    "import-dirs,library-dirs,dynamic-library-dirs,"
    "hs-libraries,extra-libraries,include-dirs,"
    "ld-options,depends"
)

# ---------------------------------------------------------------------------
# ghc-pkg helpers
# ---------------------------------------------------------------------------

def pkg_info(unit_id, db):
    """
    Query ghc-pkg field --ipid for a specific unit ID in one database.
    Returns a dict with resolved absolute paths, or None if not found.
    """
    result = subprocess.run(
        [GHC_PKG, "--package-db", db, "field", "--ipid", unit_id, INFO_FIELDS],
        capture_output=True, text=True
    )
    if result.returncode != 0 or not result.stdout.strip():
        return None
    return parse_fields(result.stdout, unit_id)

def find_pkg(unit_id):
    """Try all databases; return (info_dict, db_path) or None."""
    for db in ALL_DBS:
        if not os.path.isdir(db):
            continue
        info = pkg_info(unit_id, db)
        if info:
            return (info, db)
    return None

def parse_fields(text, unit_id):
    """Parse multi-line 'ghc-pkg field' output into a dict."""
    result = {}
    current_key = None
    current_lines = []

    for line in text.splitlines():
        m = re.match(r'^([\w-]+):\s*(.*)', line)
        if m:
            if current_key:
                result[current_key] = " ".join(current_lines).strip()
            current_key = m.group(1)
            current_lines = [m.group(2)]
        elif line and (line[0] == ' ' or line[0] == '\t') and current_key:
            current_lines.append(line.strip())

    if current_key:
        result[current_key] = " ".join(current_lines).strip()

    result.setdefault('id', unit_id)
    return result

# ---------------------------------------------------------------------------
# Package collection
# ---------------------------------------------------------------------------

def pkg_name(unit_id):
    """Extract base package name from a unit ID."""
    m = re.match(r'^([A-Za-z][A-Za-z0-9_-]*?)-\d', unit_id)
    return m.group(1) if m else unit_id

def collect_packages(root_unit_ids):
    """
    Walk the dependency graph from root_unit_ids.
    Returns dict of unit_id -> (info, db_path) | None.
    """
    visited = {}
    queue = list(root_unit_ids)

    while queue:
        uid = queue.pop()
        if uid in visited:
            continue

        name = pkg_name(uid)
        if name in SKIP_PACKAGES or uid.endswith('-inplace'):
            visited[uid] = None
            continue
        if name in BUCK2_PACKAGES:
            visited[uid] = None
            continue

        found = find_pkg(uid)
        if found is None:
            print(f"  WARNING: not found: {uid}", file=sys.stderr)
            visited[uid] = None
            continue

        info, db = found
        visited[uid] = (info, db)

        for dep_uid in info.get('depends', '').split():
            if dep_uid and dep_uid not in visited:
                queue.append(dep_uid)

    return visited

def get_root_dep_ids():
    """Read dep IDs from the fb-util and mangle inplace conf files."""
    root = set()
    for conf_name in ['fb-util-0.2.0.1-inplace.conf', 'mangle-0.1.0.1-inplace.conf']:
        path = os.path.join(INPLACE_DB, conf_name)
        if not os.path.exists(path):
            print(f"WARNING: {path} not found - run 'cabal build fb-util mangle' first",
                  file=sys.stderr)
            continue
        with open(path) as f:
            content = f.read()
        m = re.search(r'^depends:\s*(.*?)(?=^\S|\Z)', content, re.MULTILINE | re.DOTALL)
        if m:
            for uid in m.group(1).split():
                root.add(uid)
    return root

# ---------------------------------------------------------------------------
# Package database with absolute-path conf files
# ---------------------------------------------------------------------------

def is_global_pkg(uid):
    """GHC ships global packages with plain name-version IDs (no hash suffix)."""
    return not re.search(r'-[0-9a-f]{20,}$', uid)

def write_conf_file(uid, info, path):
    """
    Write a minimal GHC package conf file with absolute paths.
    Using absolute paths avoids any ${pkgroot} expansion issues.
    """
    full_id = info.get('id', uid).strip()
    lines = [
        f"name: {info.get('name', '')}",
        f"version: {info.get('version', '')}",
        f"visibility: public",
        f"id: {full_id}",
        f"key: {full_id}",
        f"license: BSD-3-Clause",
        f"exposed: True",
    ]

    for field in ('exposed-modules', 'hidden-modules'):
        val = info.get(field, '').strip()
        if val:
            lines.append(f"{field}: {val}")

    for field in ('import-dirs', 'library-dirs', 'dynamic-library-dirs',
                  'hs-libraries', 'extra-libraries', 'include-dirs'):
        val = info.get(field, '').strip()
        lines.append(f"{field}: {val}")

    # ld-options may span multiple words; preserve as-is
    ld_opts = info.get('ld-options', '').strip()
    if ld_opts:
        lines.append(f"ld-options: {ld_opts}")

    # depends: one line per dep
    depends = info.get('depends', '').split()
    if depends:
        lines.append("depends:")
        for dep in depends:
            lines.append(f"    {dep}")
    else:
        lines.append("depends:")

    lines.append("")
    with open(path, 'w') as f:
        f.write("\n".join(lines))

def setup_db(packages):
    """Create third-party/haskell/db/ with absolute-path conf files."""
    if os.path.exists(TARGET_DB):
        shutil.rmtree(TARGET_DB)
    os.makedirs(TARGET_DB)

    for uid, val in packages.items():
        if val is None:
            continue
        info, _db = val
        conf_path = os.path.join(TARGET_DB, f"{uid}.conf")
        write_conf_file(uid, info, conf_path)

    # Build the package cache
    subprocess.run(
        [GHC_PKG, "--package-db", TARGET_DB, "recache"],
        check=True
    )
    count = sum(1 for v in packages.values() if v is not None)
    print(f"  Wrote {count} conf files + recached")

# ---------------------------------------------------------------------------
# Library symlinks
# ---------------------------------------------------------------------------

TARGET_SHARED_LIBS = os.path.join(TARGET_DIR, "shared-libs")

def setup_lib_symlinks(packages):
    """Create symlinks to .a and .so files."""
    for d in (TARGET_LIBS, TARGET_SHARED_LIBS):
        if os.path.exists(d):
            shutil.rmtree(d)
        os.makedirs(d)

    lib_map = {}     # uid -> [static symlink_name, ...]
    shared_map = {}  # uid -> {soname: symlink_name}
    used_static = set()
    used_shared = set()
    GHC_VER = GHC_VERSION

    for uid, val in sorted(packages.items()):
        if val is None:
            lib_map[uid] = []
            shared_map[uid] = {}
            continue

        info, _db = val
        lib_dirs     = info.get('library-dirs', '').split()
        dyn_lib_dirs = info.get('dynamic-library-dirs', '').split() or lib_dirs
        hs_libs      = info.get('hs-libraries', '').split()

        static_links = []
        so_links = {}

        for lib_stem in hs_libs:
            # Static .a
            filename = f"lib{lib_stem}.a"
            for lib_dir in lib_dirs:
                src = os.path.join(lib_dir, filename)
                if os.path.exists(src):
                    link_name = filename
                    if link_name in used_static:
                        link_name = f"{uid}_{filename}"
                    os.symlink(src, os.path.join(TARGET_LIBS, link_name))
                    used_static.add(link_name)
                    static_links.append(link_name)
                    break
            else:
                print(f"  WARNING: .a not found for {lib_stem} in {lib_dirs}",
                      file=sys.stderr)

            # Dynamic .so
            so_filename = f"lib{lib_stem}-ghc{GHC_VER}.so"
            search_dirs = list(dict.fromkeys(dyn_lib_dirs + lib_dirs))  # unique, dyn first
            for lib_dir in search_dirs:
                src = os.path.join(lib_dir, so_filename)
                if os.path.exists(src):
                    link_name = so_filename
                    if link_name in used_shared:
                        link_name = f"{uid}_{so_filename}"
                    os.symlink(src, os.path.join(TARGET_SHARED_LIBS, link_name))
                    used_shared.add(link_name)
                    so_links[so_filename] = link_name  # soname -> symlink path
                    break

        lib_map[uid] = static_links
        shared_map[uid] = so_links

    return lib_map, shared_map

# ---------------------------------------------------------------------------
# BUCK file
# ---------------------------------------------------------------------------

def generate_buck_file(packages, lib_map, shared_map):
    # Build uid -> rule_name mapping using actual Haskell package name from info
    uid_to_rule = {}
    for uid, val in packages.items():
        if val is None:
            continue
        info, _db = val
        uid_to_rule[uid] = info.get('name', pkg_name(uid))

    lines = [
        '# @generated by mk/gen-haskell-prebuilt.py',
        '# Re-run the script to update.',
        '',
    ]

    for uid, val in sorted(packages.items()):
        if val is None:
            continue

        info, _db = val
        target  = uid_to_rule[uid]
        version = info.get('version', '')
        full_id = info.get('id', uid).strip()
        libs    = lib_map.get(uid, [])
        so_map  = shared_map.get(uid, {})

        dep_targets = []
        for dep_uid in info.get('depends', '').split():
            dep_name = pkg_name(dep_uid)
            if dep_name in SKIP_PACKAGES:
                continue
            if dep_name in BUCK2_PACKAGES:
                dep_targets.append(BUCK2_PACKAGES[dep_name])
            elif dep_uid in uid_to_rule:
                dep_targets.append(f":{uid_to_rule[dep_uid]}")

        lines.append('haskell_prebuilt_library(')
        lines.append(f'    name = {target!r},')
        lines.append(f'    version = {version!r},')
        lines.append(f'    id = {full_id!r},')
        lines.append(f'    db = "db",')
        if libs:
            lines.append('    static_libs = [')
            for lib in libs:
                lines.append(f'        "libs/{lib}",')
            lines.append('    ],')
        else:
            lines.append('    static_libs = [],')
        if so_map:
            lines.append('    shared_libs = {')
            for soname, link in sorted(so_map.items()):
                lines.append(f'        {soname!r}: "shared-libs/{link}",')
            lines.append('    },')
        else:
            lines.append('    shared_libs = {},')
        if dep_targets:
            lines.append('    deps = [')
            for t in sorted(set(dep_targets)):
                lines.append(f'        {t!r},')
            lines.append('    ],')
        else:
            lines.append('    deps = [],')
        lines.append('    visibility = ["PUBLIC"],')
        lines.append(')')
        lines.append('')

    buck_path = os.path.join(TARGET_DIR, 'BUCK')
    with open(buck_path, 'w') as f:
        f.write('\n'.join(lines) + '\n')
    count = sum(1 for v in packages.values() if v is not None)
    print(f"  Generated {buck_path} ({count} rules)")

# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    os.makedirs(TARGET_DIR, exist_ok=True)

    print("Reading root dep IDs...")
    root_ids = get_root_dep_ids()
    print(f"  {len(root_ids)} root deps")

    print("Resolving transitive dependencies...")
    packages = collect_packages(root_ids)
    found   = sum(1 for v in packages.values() if v is not None)
    skipped = len(packages) - found
    print(f"  {found} resolved, {skipped} skipped/buck2")

    print("Building package database...")
    setup_db(packages)

    print("Creating library symlinks...")
    lib_map, shared_map = setup_lib_symlinks(packages)
    static_count = sum(len(v) for v in lib_map.values())
    shared_count = sum(len(v) for v in shared_map.values())
    print(f"  {static_count} .a symlinks, {shared_count} .so symlinks")

    print("Generating BUCK file...")
    generate_buck_file(packages, lib_map, shared_map)

    print("Done.")

if __name__ == '__main__':
    main()
