#!/usr/bin/env python3
"""
Generate haskell_prebuilt_library() rules for Cabal dependencies.

Uses 'ghc-pkg field --ipid' to resolve each package by its exact unit ID,
then generates:

  third-party/haskell/
    store-db/    Filtered GHC package DB for cabal-store packages
                 (conf files symlinked from cabal store; recached here)
    BUCK         haskell_prebuilt_library() rules
    ghc-<version>, cabal-store   repo symlinks to the real GHC/cabal-store
                 roots (created/updated by this script - see
                 ensure_symlink()), so BUCK rules can reference them by a
                 repo-relative path instead of a host-specific absolute one

Which GHC version all of this targets is read from Cabal's own
`dist-newstyle/cache/plan.json` (`compiler-id`), not hardcoded here - so
retargeting the whole buck2 build at a different GHC version is just:

  cabal build all --only-dependencies -w ghc-<version>
  buck2/gen-haskell-prebuilt.py

Run from the Glean repository root.
"""

import glob
import json
import os
import re
import shutil
import subprocess
import sys

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------

GLEAN_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

def _read_ghc_version():
    """The GHC version Cabal last resolved this project's build plan
    against (`dist-newstyle/cache/plan.json`'s own `compiler-id`, e.g.
    "ghc-9.4.8") - written by `cabal build all --only-dependencies
    [-w ghc-<version>]`. Reading it from there, rather than hardcoding
    it, is what makes re-running that command with a different `-w` and
    then this script enough to retarget everything downstream at a
    different GHC version.
    """
    plan_path = os.path.join(GLEAN_ROOT, "dist-newstyle/cache/plan.json")
    if not os.path.exists(plan_path):
        print(f"ERROR: {plan_path} not found - run "
              f"'cabal build all --only-dependencies' first", file=sys.stderr)
        sys.exit(1)
    with open(plan_path) as f:
        plan = json.load(f)
    compiler_id = plan['compiler-id']
    if not compiler_id.startswith('ghc-'):
        print(f"ERROR: unexpected compiler-id {compiler_id!r} in {plan_path} "
              f"(expected 'ghc-<version>')", file=sys.stderr)
        sys.exit(1)
    return compiler_id[len('ghc-'):]

GHC_VERSION = _read_ghc_version()

def _find_ghc():
    """Find the `ghc` binary for GHC_VERSION on $PATH - tries the
    version-suffixed convention most GHC install methods provide (e.g.
    "ghc-9.4.8": ghcup, distro packages, Nix profiles, ...), then falls
    back to a bare "ghc" if it happens to already be that exact version
    (e.g. a Nix devshell with exactly one GHC in scope, unsuffixed) -
    never silently accepts a mismatched one. Deliberately doesn't assume
    any particular install layout (no `~/.ghcup/...` or similar) - only
    that *some* `ghc[-<version>]` is reachable on $PATH, which is true
    regardless of how it got there.
    """
    versioned = shutil.which(f"ghc-{GHC_VERSION}")
    if versioned:
        return versioned
    bare = shutil.which("ghc")
    if bare:
        result = subprocess.run([bare, "--numeric-version"], capture_output=True, text=True)
        if result.returncode == 0 and result.stdout.strip() == GHC_VERSION:
            return bare
    print(f"ERROR: no 'ghc-{GHC_VERSION}' (or a bare 'ghc' of that exact "
          f"version) found on $PATH", file=sys.stderr)
    sys.exit(1)

def _find_ghc_pkg(ghc_path):
    """`ghc-pkg` is always installed as a sibling of `ghc` itself, using
    the same naming convention (versioned or bare) - deriving it this
    way, rather than searching $PATH independently, guarantees it's
    genuinely the matching one rather than some other ghc-pkg that
    happens to be on $PATH.
    """
    ghc_dir, ghc_name = os.path.split(ghc_path)
    pkg_name = ghc_name.replace("ghc", "ghc-pkg", 1)
    candidate = os.path.join(ghc_dir, pkg_name)
    if not os.path.exists(candidate):
        print(f"ERROR: expected '{candidate}' (alongside '{ghc_path}') to exist",
              file=sys.stderr)
        sys.exit(1)
    return candidate

def _ghc_print(ghc_path, flag):
    result = subprocess.run([ghc_path, flag], capture_output=True, text=True)
    if result.returncode != 0:
        print(f"ERROR: '{ghc_path} {flag}' failed:\n{result.stderr}", file=sys.stderr)
        sys.exit(1)
    return result.stdout.strip()

GHC = _find_ghc()
GHC_PKG = _find_ghc_pkg(GHC)

# GHC's own reported lib directory is the stable anchor for everything it
# bundles - both the global package db (a direct child, `<libdir>/
# package.conf.d`) and every boot package's own library-dirs/include-dirs
# (also direct children, e.g. `<libdir>/base-4.17.2.1/...` - confirmed by
# reading a real `ghc-pkg field base library-dirs`, which literally
# resolves to a `<libdir>/../lib/...` path that cancels straight back
# down to a `<libdir>/...` one). Asking GHC itself for this (rather than
# assuming a directory layout for however it was installed) is what
# removes the ghcup-specific assumption this used to bake in.
GLOBAL_ROOT_ABS = os.path.realpath(_ghc_print(GHC, "--print-libdir"))
GLOBAL_DB = os.path.join(GLOBAL_ROOT_ABS, "package.conf.d")

# Wherever `ghc` itself was actually found (see _find_ghc()) - this is
# *not* necessarily anywhere near GLOBAL_ROOT_ABS (a distro package, for
# instance, commonly puts the binary in /usr/bin and the libdir in
# /usr/lib/ghc-<version>, two unrelated trees) - so it needs its own repo
# symlink, separate from the libdir one, for anything that needs GHC's
# own `bin/` on `$PATH` (e.g. a test-suite working around the hie-indexer
# GHC-version mismatch - see glean/lang/haskell/tests/BUCK).
GHC_BIN_ABS = os.path.dirname(os.path.realpath(GHC))

def _find_store_roots():
    """Cabal's per-compiler store directory isn't reliably just "ghc-
    <version>" - some cabal-install/GHC combinations (observed: GHC 9.8.2
    with cabal-install 3.14) suffix it with an ABI hash instead, e.g.
    "ghc-9.8.2-6af5", to keep incompatible builds of the same nominal
    version from colliding. Cabal doesn't expose that suffix through any
    query command (`cabal path --store-dir` only gives the unversioned
    `~/.cabal/store` root) - discovering it by construction would mean
    reverse-engineering an internal, undocumented hash. Globbing instead
    (any directory starting with "ghc-<version>") sidesteps needing to
    know the exact suffix at all, and self-corrects if the naming scheme
    changes again - find_pkg() tries every match for each package
    individually, rather than this needing to guess the one true root
    ahead of time.
    """
    base = os.path.expanduser("~/.cabal/store")
    prefix = f"ghc-{GHC_VERSION}"
    roots = sorted(
        d for d in glob.glob(os.path.join(base, prefix + "*"))
        if os.path.isdir(d) and (os.path.basename(d) == prefix or os.path.basename(d).startswith(prefix + "-"))
    )
    if not roots:
        print(f"ERROR: no '{base}/{prefix}*' directory found - run "
              f"'cabal build all --only-dependencies -w {GHC}' first",
              file=sys.stderr)
        sys.exit(1)
    return roots

STORE_ROOTS = _find_store_roots()
STORE_DBS   = [os.path.join(r, "package.db") for r in STORE_ROOTS]
INPLACE_DB  = os.path.join(GLEAN_ROOT, f"dist-newstyle/packagedb/ghc-{GHC_VERSION}")
ALL_DBS     = [GLOBAL_DB] + STORE_DBS + [INPLACE_DB]

TARGET_DIR    = os.path.join(GLEAN_ROOT, "third-party/haskell")
TARGET_STORE_DB = os.path.join(TARGET_DIR, "store-db")

# Repo-relative db paths (relative to TARGET_DIR) used in BUCK rules
GLOBAL_DB_REL = f"ghc-{GHC_VERSION}/package.conf.d"
STORE_DB_REL  = "store-db"

# Absolute roots for translating absolute paths -> repo-relative. One
# real store root per STORE_DBS entry (see _find_store_roots()) - in
# practice only one of these ever actually has any of our packages in it
# (see main()'s own check when picking which one "cabal-store" should
# point at), but abs_to_rel() tries all of them, same as find_pkg() does
# for lookups.
STORE_ROOTS_ABS = [os.path.realpath(r) for r in STORE_ROOTS]
GLOBAL_ROOT_REL = f"ghc-{GHC_VERSION}"
STORE_ROOT_REL  = "cabal-store"
GHC_BIN_REL     = "ghc-bin"

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
    for store_root in STORE_ROOTS_ABS:
        if p.startswith(store_root + "/"):
            return STORE_ROOT_REL + "/" + p[len(store_root) + 1:]
    return None

def is_global_pkg(uid):
    """GHC global packages have plain name-version IDs (no hash suffix)."""
    return not re.search(r'-[0-9a-f]{20,}$', uid)

def ensure_symlink(link_path, target_path):
    """Point `link_path` at `target_path`, creating or repointing it as
    needed - so switching GHC versions doesn't leave behind a symlink
    (e.g. `third-party/haskell/ghc-9.4.8`) still named after the old one.
    """
    if os.path.islink(link_path) and os.readlink(link_path) == target_path:
        return
    if os.path.lexists(link_path):
        os.remove(link_path)
    os.symlink(target_path, link_path)

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
            ["cabal", "list-bin", "-w", GHC, tool],
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
        '# @generated by buck2/gen-haskell-prebuilt.py',
        '# Re-run the script to update.',
        '',
        # Exported so nothing else (buck2/toolchains/BUCK's compiler/
        # packager names, or a BUCK file needing this GHC's own `bin/` on
        # $PATH at test time) has to hardcode a GHC version that only this
        # script - reading it fresh from Cabal's plan.json - actually
        # knows.
        f'GHC_VERSION = {GHC_VERSION!r}',
        f'GHC_BIN_DIR = "third-party/haskell/{GHC_BIN_REL}"',
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
        if db not in STORE_DBS:
            continue
        # Symlink the .conf file from the real store DB (whichever of
        # STORE_DBS this particular package actually resolved from).
        src  = os.path.join(db, f"{uid}.conf")
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
    if db_path in STORE_DBS:
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
        '# @generated by buck2/gen-haskell-prebuilt.py',
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
    # Drop any other "ghc-<version>" symlink left over from a previous run
    # targeting a different GHC version - otherwise switching versions
    # just accumulates stale, unused ones (harmless, but pointless cruft
    # in a directory meant to be fully regenerated) rather than actually
    # switching. `GHC_BIN_REL` ("ghc-bin") also starts with "ghc-" but
    # isn't itself version-named, so it's excluded alongside the current
    # version's own symlink.
    for entry in os.listdir(TARGET_DIR):
        entry_path = os.path.join(TARGET_DIR, entry)
        if (entry not in (GLOBAL_ROOT_REL, GHC_BIN_REL) and entry.startswith("ghc-")
                and os.path.islink(entry_path)):
            os.remove(entry_path)

    ensure_symlink(os.path.join(TARGET_DIR, GLOBAL_ROOT_REL), GLOBAL_ROOT_ABS)
    ensure_symlink(os.path.join(TARGET_DIR, GHC_BIN_REL), GHC_BIN_ABS)

    print("Reading root dep IDs...")
    root_ids = get_root_dep_ids()
    print(f"  {len(root_ids)} root deps")

    print("Resolving transitive dependencies...")
    packages = collect_packages(root_ids)
    found   = sum(1 for v in packages.values() if v is not None)
    skipped = len(packages) - found
    print(f"  {found} resolved, {skipped} skipped (local or not found)")

    # Point "cabal-store" at whichever of STORE_ROOTS actually turned out
    # to hold our packages - there can be more than one candidate (see
    # _find_store_roots()), but everything we found should have come from
    # exactly one of them in practice (they're alternate ABI-hash variants
    # of the same nominal GHC version, not meant to be mixed within one
    # resolved build). More than one actually in use means something more
    # confusing is going on than this script can safely guess its way
    # through - worth a human looking, not a silent pick.
    used_roots = {db for _, db in packages.values() if db in STORE_DBS}
    if len(used_roots) > 1:
        print(f"ERROR: packages resolved from more than one cabal store "
              f"directory: {sorted(used_roots)} - expected at most one",
              file=sys.stderr)
        sys.exit(1)
    store_root = os.path.dirname(next(iter(used_roots))) if used_roots else os.path.dirname(STORE_DBS[0])
    ensure_symlink(os.path.join(TARGET_DIR, STORE_ROOT_REL), store_root)

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
