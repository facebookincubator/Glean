#!/usr/bin/env python3
"""
Generate haskell_prebuilt_library() rules for Cabal dependencies.

Uses 'ghc-pkg field --ipid' to resolve each package by its exact unit ID,
then generates:

  third-party/haskell/
    store-db/    Filtered GHC package DB for cabal-store packages
                 (conf files symlinked from cabal store; recached here)
    BUCK         haskell_prebuilt_library() rules

Global GHC packages (base, parsec, etc.) are served via:
  third-party/haskell/ghc-9.4.8  ->  ~/.ghcup/ghc/9.4.8   (repo symlink)

Store packages are served via:
  third-party/haskell/cabal-store ->  ~/.cabal/store/ghc-9.4.8  (repo symlink)

Run from the Glean repository root.
"""

import json
import os
import re
import shutil
import subprocess
import sys

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------

GHC_VERSION = "9.4.8"
GHC_PKG = os.path.expanduser(f"~/.ghcup/ghc/{GHC_VERSION}/bin/ghc-pkg")
GLEAN_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

GLOBAL_DB  = os.path.expanduser(
    f"~/.ghcup/ghc/{GHC_VERSION}/lib/ghc-{GHC_VERSION}/lib/package.conf.d"
)
STORE_DB   = os.path.expanduser(f"~/.cabal/store/ghc-{GHC_VERSION}/package.db")
INPLACE_DB = os.path.join(GLEAN_ROOT, f"dist-newstyle/packagedb/ghc-{GHC_VERSION}")
ALL_DBS    = [GLOBAL_DB, STORE_DB, INPLACE_DB]

TARGET_DIR    = os.path.join(GLEAN_ROOT, "third-party/haskell")
TARGET_STORE_DB = os.path.join(TARGET_DIR, "store-db")

# Repo-relative db paths (relative to TARGET_DIR) used in BUCK rules
GLOBAL_DB_REL = f"ghc-{GHC_VERSION}/lib/ghc-{GHC_VERSION}/lib/package.conf.d"
STORE_DB_REL  = "store-db"

# Absolute roots for translating absolute paths -> repo-relative
GLOBAL_ROOT_ABS = os.path.realpath(os.path.expanduser(f"~/.ghcup/ghc/{GHC_VERSION}"))
STORE_ROOT_ABS  = os.path.realpath(os.path.expanduser(f"~/.cabal/store/ghc-{GHC_VERSION}"))
GLOBAL_ROOT_REL = f"ghc-{GHC_VERSION}"
STORE_ROOT_REL  = "cabal-store"

# Build tools that, unlike hsc2hs, don't ship with GHC - they're ordinary
# Cabal packages. We ask Cabal to build them and tell us where, rather than
# hardcoding a cabal-store path (which includes a hash that changes
# whenever the solved version changes).
BUILD_TOOLS = ["alex", "happy"]

INFO_FIELDS = "name,version,id,library-dirs,dynamic-library-dirs,hs-libraries,depends,include-dirs"

# ---------------------------------------------------------------------------
# Path helpers
# ---------------------------------------------------------------------------

def abs_to_rel(abs_path):
    """Convert absolute path under GHC or cabal-store to repo-relative (via symlinks)."""
    p = os.path.realpath(abs_path)
    if p.startswith(GLOBAL_ROOT_ABS + "/"):
        return GLOBAL_ROOT_REL + "/" + p[len(GLOBAL_ROOT_ABS) + 1:]
    if p.startswith(STORE_ROOT_ABS + "/"):
        return STORE_ROOT_REL + "/" + p[len(STORE_ROOT_ABS) + 1:]
    return None

def is_global_pkg(uid):
    """GHC global packages have plain name-version IDs (no hash suffix)."""
    return not re.search(r'-[0-9a-f]{20,}$', uid)

# ---------------------------------------------------------------------------
# ghc-pkg helpers
# ---------------------------------------------------------------------------

def pkg_info(unit_id, db):
    result = subprocess.run(
        [GHC_PKG, "--package-db", db, "field", "--ipid", unit_id, INFO_FIELDS],
        capture_output=True, text=True
    )
    if result.returncode != 0 or not result.stdout.strip():
        return None
    return parse_fields(result.stdout, unit_id)

def find_pkg(unit_id):
    for db in ALL_DBS:
        if not os.path.isdir(db):
            continue
        info = pkg_info(unit_id, db)
        if info:
            return (info, db)
    return None

def parse_fields(text, unit_id):
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
        elif line and (line[0] in ' \t') and current_key:
            current_lines.append(line.strip())
    if current_key:
        result[current_key] = " ".join(current_lines).strip()
    result.setdefault('id', unit_id)
    return result

# ---------------------------------------------------------------------------
# Package collection
# ---------------------------------------------------------------------------

def pkg_name(unit_id):
    m = re.match(r'^([A-Za-z][A-Za-z0-9_-]*?)-\d', unit_id)
    return m.group(1) if m else unit_id

def collect_packages(root_ids):
    visited = {}
    queue = list(root_ids)
    while queue:
        uid = queue.pop()
        if uid in visited:
            continue
        name = pkg_name(uid)
        # A simple package's inplace unit-id is "<pkg>-<ver>-inplace"; a
        # named-sublibrary one (e.g. glean.cabal.in's `library stubs`) is
        # "<pkg>-<ver>-inplace-<sublib>" - neither is a real installed
        # package buck2 can reference by store path (it's one of this
        # project's own local packages, built directly by buck2 rather
        # than vendored as a vendored prebuilt). get_root_dep_ids() should
        # never hand this walk a local id as a *root* (it filters using
        # plan.json's own local/external split), and no genuinely external
        # package's own `depends:` should ever reference one either - this
        # check is a cheap, generic backstop for both, not something that
        # should ordinarily fire.
        if '-inplace' in uid:
            visited[uid] = None
            continue
        found = find_pkg(uid)
        if found is None:
            print(f"  WARNING: not found: {uid}", file=sys.stderr)
            visited[uid] = None
            continue
        info, db = found
        visited[uid] = (info, db)
        for dep in info.get('depends', '').split():
            if dep and dep not in visited:
                queue.append(dep)
    return visited

def get_root_dep_ids():
    """Every package ID any local component depends on, that isn't itself
    a local component - i.e. every genuinely external (Hackage/system)
    package this project's own code needs, anywhere in the project,
    regardless of which package or component declares the dependency.

    Derived entirely from Cabal's own `dist-newstyle/cache/plan.json`,
    which `cabal build all --only-dependencies` populates with a
    `style: "local"` entry for *every* component of *every* package
    listed in `cabal.project` (library, executable and test-suite alike -
    this project's `tests: True` is what pulls test-suites in too) -
    without ever actually building any of them (dependency *resolution*
    is a static solve over each package's own `build-depends:` field,
    independent of compilation - see buck2.md's "explore a build
    reconfigured around Cabal" entry for how this was confirmed
    empirically). This is why nothing project-specific needs to be
    hand-maintained here any more: no per-package .conf filename list, no
    wanted-component allowlist, no manual roots for a package (like
    glean-clang's `clang-derive-lib`) that only *some* other local
    component happens to need - every local component's own `depends:`
    is walked, uniformly.

    (One thing this genuinely can't discover: extra C-library flags from
    a component's `pkgconfig-depends:` - e.g. `rts`'s icu-uc/gflags or
    glean-clang's LLVM linkage - since those only get computed when Cabal
    runs a package's *real* configure step, which `--only-dependencies`
    skips for every local component. That's a separate, narrower problem
    from root-dependency discovery, needing its own solution.)
    """
    plan_path = os.path.join(GLEAN_ROOT, "dist-newstyle/cache/plan.json")
    if not os.path.exists(plan_path):
        print(f"ERROR: {plan_path} not found - run "
              f"'cabal build all --only-dependencies' first", file=sys.stderr)
        sys.exit(1)
    with open(plan_path) as f:
        plan = json.load(f)

    install_plan = plan['install-plan']
    local_ids = {c['id'] for c in install_plan if c.get('style') == 'local'}

    root = set()
    for c in install_plan:
        if c.get('style') != 'local':
            continue
        for uid in c.get('depends', []):
            if uid not in local_ids:
                root.add(uid)
    return root


# ---------------------------------------------------------------------------
# Build tools (alex, happy, ...)
# ---------------------------------------------------------------------------

def get_tool_paths():
    """Ask Cabal where it put each of BUILD_TOOLS, as repo-relative paths."""
    paths = {}
    for tool in BUILD_TOOLS:
        result = subprocess.run(
            ["cabal", "list-bin", tool],
            cwd=GLEAN_ROOT, capture_output=True, text=True
        )
        if result.returncode != 0 or not result.stdout.strip():
            print(f"  WARNING: 'cabal list-bin {tool}' failed - "
                  f"run 'cabal build {tool}' first", file=sys.stderr)
            continue
        abs_path = result.stdout.strip()
        rel = abs_to_rel(abs_path)
        if rel is None:
            print(f"  WARNING: {tool} path {abs_path} isn't under "
                  f"cabal-store or ghc-{GHC_VERSION}", file=sys.stderr)
            continue
        paths[tool] = rel
    return paths

def generate_tools_file(tool_paths):
    lines = [
        '# @generated by mk/gen-haskell-prebuilt.py',
        '# Re-run the script to update.',
        '',
    ]
    for tool in BUILD_TOOLS:
        rel = tool_paths.get(tool)
        if rel is not None:
            lines.append(f'{tool.upper()} = "third-party/haskell/{rel}"')

    tools_path = os.path.join(TARGET_DIR, 'tools.bzl')
    with open(tools_path, 'w') as f:
        f.write('\n'.join(lines) + '\n')
    print(f"  Generated {tools_path} ({len(tool_paths)} tools)")

# ---------------------------------------------------------------------------
# Filtered store DB
# ---------------------------------------------------------------------------

def setup_store_db(packages):
    """
    Create store-db/ with symlinks to .conf files for our exact store packages,
    then recache so ghc-pkg can use it.
    """
    if os.path.exists(TARGET_STORE_DB):
        shutil.rmtree(TARGET_STORE_DB)
    os.makedirs(TARGET_STORE_DB)

    count = 0
    for uid, val in packages.items():
        if val is None or is_global_pkg(uid):
            continue
        info, db = val
        if db != STORE_DB:
            continue
        # Symlink the .conf file from the real store DB
        src  = os.path.join(STORE_DB, f"{uid}.conf")
        dest = os.path.join(TARGET_STORE_DB, f"{uid}.conf")
        if os.path.exists(src):
            os.symlink(src, dest)
            count += 1
        else:
            print(f"  WARNING: no .conf for {uid} in store", file=sys.stderr)

    subprocess.run([GHC_PKG, "--package-db", TARGET_STORE_DB, "recache"], check=True)
    print(f"  Symlinked {count} store .conf files + recached")

# ---------------------------------------------------------------------------
# BUCK file
# ---------------------------------------------------------------------------

def db_rel_for(uid, db_path):
    if is_global_pkg(uid):
        return GLOBAL_DB_REL
    if db_path == STORE_DB:
        return STORE_DB_REL
    return None

def generate_buck_file(packages):
    uid_to_rule = {}
    for uid, val in packages.items():
        if val is None:
            continue
        info, _ = val
        uid_to_rule[uid] = info.get('name', pkg_name(uid))

    lines = [
        '# @generated by mk/gen-haskell-prebuilt.py',
        '# Re-run the script to update.',
        '',
    ]

    for uid, val in sorted(packages.items()):
        if val is None:
            continue

        info, db_path = val
        db_rel = db_rel_for(uid, db_path)
        if db_rel is None:
            continue

        target  = uid_to_rule[uid]
        version = info.get('version', '')
        full_id = info.get('id', uid).strip()

        lib_dirs     = info.get('library-dirs', '').split()
        dyn_lib_dirs = info.get('dynamic-library-dirs', '').split() or lib_dirs
        hs_libs      = info.get('hs-libraries', '').split()

        static_libs = []
        shared_libs = {}
        for stem in hs_libs:
            for d in lib_dirs:
                src = os.path.join(d, f"lib{stem}.a")
                if os.path.exists(src):
                    rel = abs_to_rel(src)
                    if rel:
                        static_libs.append(rel)
                    break
            else:
                print(f"  WARNING: .a not found for {stem}", file=sys.stderr)

            soname = f"lib{stem}-ghc{GHC_VERSION}.so"
            for d in list(dict.fromkeys(dyn_lib_dirs + lib_dirs)):
                src = os.path.join(d, soname)
                if os.path.exists(src):
                    rel = abs_to_rel(src)
                    if rel:
                        shared_libs[soname] = rel
                    break

        header_dirs = []
        for d in info.get('include-dirs', '').split():
            rel = abs_to_rel(d)
            if rel:
                header_dirs.append(rel)

        dep_targets = []
        for dep_uid in info.get('depends', '').split():
            if dep_uid in uid_to_rule:
                dep_targets.append(f":{uid_to_rule[dep_uid]}")

        lines.append('haskell_prebuilt_library(')
        lines.append(f'    name = {target!r},')
        lines.append(f'    version = {version!r},')
        lines.append(f'    id = {full_id!r},')
        lines.append(f'    db = {db_rel!r},')
        if static_libs:
            lines.append('    static_libs = [')
            for p in static_libs:
                lines.append(f'        {p!r},')
            lines.append('    ],')
        else:
            lines.append('    static_libs = [],')
        if shared_libs:
            lines.append('    shared_libs = {')
            for soname, p in sorted(shared_libs.items()):
                lines.append(f'        {soname!r}: {p!r},')
            lines.append('    },')
        else:
            lines.append('    shared_libs = {},')
        if header_dirs:
            lines.append('    cxx_header_dirs = [')
            for p in header_dirs:
                lines.append(f'        {p!r},')
            lines.append('    ],')
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
    count = sum(1 for uid, v in packages.items()
                if v is not None and db_rel_for(uid, v[1]) is not None)
    print(f"  Generated {buck_path} ({count} rules)")

# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    for rel in [GLOBAL_ROOT_REL, STORE_ROOT_REL]:
        link = os.path.join(TARGET_DIR, rel)
        if not os.path.islink(link):
            print(f"ERROR: missing symlink {link}", file=sys.stderr)
            sys.exit(1)

    print("Reading root dep IDs...")
    root_ids = get_root_dep_ids()
    print(f"  {len(root_ids)} root deps")

    print("Resolving transitive dependencies...")
    packages = collect_packages(root_ids)
    found   = sum(1 for v in packages.values() if v is not None)
    skipped = len(packages) - found
    print(f"  {found} resolved, {skipped} skipped (local or not found)")

    print("Building filtered store-db...")
    setup_store_db(packages)

    print("Generating BUCK file...")
    generate_buck_file(packages)

    print("Locating build tools...")
    tool_paths = get_tool_paths()
    generate_tools_file(tool_paths)

    print("Done.")

if __name__ == '__main__':
    main()
