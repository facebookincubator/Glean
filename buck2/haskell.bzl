# Wrappers around the native haskell_library()/haskell_binary() rules.
#
# These handle:
#   - package deps: `packages = ["text", ...]` instead of explicit
#     `"//third-party/haskell:text"` entries in `deps`.
#   - a standard set of packages (base, rts) added to every target.
#   - the fb-haskell extension set (from the `fb-haskell` common stanza in
#     glean.cabal.in) enabled by default, so individual rules don't need to
#     repeat it. Pass fb_haskell = False for a package that doesn't import
#     that common stanza (e.g. mangle, which declares its own minimal
#     default-extensions) - compiler_flags is then used as-is instead of
#     appended to FB_HASKELL_EXTENSIONS.
#   - hsc2hs: any `.hsc` file in `srcs` is automatically preprocessed, with
#     include paths derived from `deps` (see buck2/hsc2hs.bzl) - so a `.hsc`
#     file that needs a C++ dependency's headers just needs that dependency
#     listed in `deps`, same as any other buck2 target.
#   - alex/happy: any `.x`/`.y` file in `srcs` is automatically run through
#     the corresponding tool (see buck2/alex_happy.bzl).
#   - a source living somewhere other than its module name implies (e.g. a
#     shared `plugins/` directory) is relocated with export_file() rather
#     than passed via the dict form of `srcs`, which is deprecated - see
#     `srcs` given as a dict below.
#
# Note on -threaded: it only needs to reach the final link (it selects which
# RTS to link against), not the per-module compile step, so pass it via
# linker_flags on haskell_binary(), not compiler_flags.

load("//buck2:alex_happy.bzl", "alex", "happy")
load("//buck2:hsc2hs.bzl", "hsc2hs")

# Packages implicitly needed by every Haskell target.
AUTO_PACKAGES = ["base", "rts"]

# Extensions enabled by the `fb-haskell` common stanza in glean.cabal.in.
FB_HASKELL_EXTENSIONS = [
    "-XHaskell2010",
    "-XBangPatterns",
    "-XBinaryLiterals",
    "-XDataKinds",
    "-XDeriveDataTypeable",
    "-XDeriveGeneric",
    "-XEmptyCase",
    "-XExistentialQuantification",
    "-XFlexibleContexts",
    "-XFlexibleInstances",
    "-XGADTs",
    "-XGeneralizedNewtypeDeriving",
    "-XLambdaCase",
    "-XMultiParamTypeClasses",
    "-XMultiWayIf",
    "-XNamedFieldPuns",
    "-XNoMonomorphismRestriction",
    "-XOverloadedStrings",
    "-XPatternSynonyms",
    "-XRankNTypes",
    "-XRecordWildCards",
    "-XScopedTypeVariables",
    "-XStandaloneDeriving",
    "-XTupleSections",
    "-XTypeFamilies",
    "-XTypeSynonymInstances",
    "-XNondecreasingIndentation",
    "-XTypeOperators",
]

def _package_deps(packages):
    all_pkgs = {p: None for p in (AUTO_PACKAGES + packages)}
    return [("//third-party/haskell:" + p) for p in sorted(all_pkgs.keys())]

# Build modes (buck2.md TODO "we should support different build modes"),
# selected via `buck2 build ... -m root//buck2/constraints:opt` (`dev` is
# the default - see the root PACKAGE file). `dev` matches this migration's
# original, only behaviour (shared libs, no optimisation - fast to
# rebuild); `opt` is what an actual deployed `glean` binary wants (a single
# static binary, optimised). Centralized here rather than passed by each
# BUCK file, the same reasoning as FB_HASKELL_EXTENSIONS above - one place
# to change, automatically applied to every haskell_library()/
# haskell_binary() in the tree.
_BUILD_MODE_LINK_STYLE = select({
    "root//buck2/constraints:opt": "static",
    "DEFAULT": "shared",
})

# GHC's `-O` (Cabal's own default build has no explicit -O0/-O1/-O2
# anywhere in glean.cabal.in, so `dev` matches that; `opt` turns on GHC's
# standard optimisation level).
_BUILD_MODE_HASKELL_FLAGS = select({
    "root//buck2/constraints:opt": ["-O"],
    "DEFAULT": [],
})

# `opt` mode builds everything `link_style = "static"`, but Template
# Haskell splices still need every package loadable the *dynamic* way:
# this GHC's own `ghc` binary is dynamically linked (`ghc --info` shows
# "GHC Dynamic: YES"), and its internal splice interpreter can only load
# packages that were also built the dynamic way - not a property of
# Template Haskell itself, just how this GHC binary happens to be built.
# Tried `-fexternal-interpreter` first (runs splices in a separate,
# non-dynamic `ghc-iserv` process instead), but that has two hard problems
# of its own, confirmed via direct GHC repros with no buck2 involved:
# `ghc-iserv`'s internal object loader can't handle the ELF TLS
# relocations modern C++ (e.g. folly) generates, and it eagerly loads
# *every* exposed package's native closure for the whole `ghc --make`
# session the moment any one splice needs it, not just that splice's own
# transitive deps. Using real dynamic linking instead (matching what Cabal
# already does) sidesteps both: a `.so`'s native deps resolve automatically
# via `DT_NEEDED`, and the OS dynamic linker (unlike GHC's internal one)
# handles every relocation type. See `dynamic_too` in
# `prelude/haskell/haskell.bzl` for the actual mechanism - in `opt` mode
# every haskell_library() builds its static archive and shared library
# together from a single `-dynamic-too` compile (rather than two
# independent ones, which would silently redo the dynamic-way codegen
# twice for no reason), and its package `.conf` advertises
# `dynamic-library-dirs:` pointing at the result.
_BUILD_MODE_DYNAMIC_TOO = select({
    "root//buck2/constraints:opt": True,
    "DEFAULT": False,
})

# The .hs path a source's module lives at once preprocessed: `path`
# unchanged unless it still carries a raw preprocessor extension (true for
# `srcs` given as a list, where `path == src`, or an explicit identity entry
# in the dict form), in which case that extension is stripped and replaced
# with .hs. Exported for callers (e.g. thrift_haskell_library() in
# thrift.bzl) that need to compute the same key haskell_library()/
# haskell_binary() would derive from a plain `srcs` list, to merge
# additional dict entries into it without breaking that derivation.
def hs_module_path(path):
    for ext in (".hsc", ".x", ".y"):
        if path.endswith(ext):
            return path[:-len(ext)] + ".hs"
    return path

def _resolve_src(name, path, src, deps, hsc_flags):
    # `path` is the module-derived path this source should end up at (e.g.
    # what its module name maps to); `src` is the actual file, which may
    # differ from `path` for a source living outside its module's directory
    # layout (see the dict form of `srcs`, below). Whatever we return here
    # always ends up in a plain *list* passed to the native rule - srcs as a
    # dict is deprecated - so every branch must produce an artifact whose
    # own path already matches `path`.
    out = hs_module_path(path)
    if src.endswith(".hsc"):
        rule_name = name + "-hsc-" + out.replace("/", "_")
        hsc2hs(name = rule_name, hsc_file = src, out = out, deps = deps, extra_flags = hsc_flags)
        return ":" + rule_name
    elif src.endswith(".x"):
        rule_name = name + "-alex-" + out.replace("/", "_")
        alex(name = rule_name, src = src, out = out)
        return ":" + rule_name
    elif src.endswith(".y"):
        rule_name = name + "-happy-" + out.replace("/", "_")
        happy(name = rule_name, src = src, out = out)
        return ":" + rule_name
    elif path == src or src.endswith("[" + path + "]"):
        # Already at the right path: either a real file living exactly
        # there, or a sub-target reference (e.g. from thrift_compile() via
        # thrift_haskell_library()/thrift_haskell_binary() in thrift.bzl)
        # whose bracketed key already equals `path` - its own artifact's
        # short_path is already correct, so relocating it again would just
        # be a redundant copy.
        return src
    else:
        # A real (already-.hs) source that doesn't live at its module path -
        # relocate it with export_file() (the same ctx.actions.copy_file()
        # primitive export_file.bzl itself uses), so the native rule always
        # sees a correctly-pathed source. This is what makes a module
        # registered correctly in the package db even when nothing in this
        # target imports it directly (i.e. it's only consumed by a *different*
        # target depending on this one) - confirmed empirically: a plain
        # (non-relocated) src with the wrong derived path still compiles
        # within its own target (GHC resolves same-target imports from the
        # sources' own `module X where` headers, not buck2's bookkeeping),
        # but a cross-target `import` of it fails, since haskell_library()
        # registers the *derived* path as the exposed module name.
        rule_name = name + "-mv-" + path.replace("/", "_")
        native.export_file(name = rule_name, src = src, out = path)
        return ":" + rule_name

# `srcs` is usually a list, where each file's own path (relative to this
# BUCK package) determines its module name. A dict `{modulePath: file}` is
# also accepted for the rare case where a source doesn't live at the path
# its module name implies (e.g. a shared `plugins/` directory holding
# modules that belong under the main package's namespace) - internally
# resolved to a plain list (see _resolve_src) since dict-form srcs on the
# native rule is deprecated.
def _resolve_srcs(name, srcs, deps, hsc_flags):
    items = srcs.items() if type(srcs) == type({}) else [(src, src) for src in srcs]
    return [_resolve_src(name, path, src, deps, hsc_flags) for path, src in items]

def haskell_library(
        name,
        srcs = [],
        packages = [],
        deps = [],
        compiler_flags = [],
        fb_haskell = True,
        # Extra -C-style flags for every .hsc file in `srcs` (see
        # hsc2hs.bzl's `extra_flags`) - the buck2 equivalent of Cabal's
        # per-library `hsc2hs-options` field.
        hsc_flags = [],
        **kwargs):
    # No `link_style` here - unlike haskell_binary(), haskell_library()
    # doesn't take one at all: a library builds whichever output styles its
    # `preferred_linkage` calls for (both, by default), and it's entirely
    # the *consumer* doing the linking (ultimately some haskell_binary())
    # that picks which one to actually use. The build-mode link_style only
    # needs to be set once, there.
    all_deps = deps + _package_deps(packages)
    all_compiler_flags = (FB_HASKELL_EXTENSIONS + compiler_flags) if fb_haskell else compiler_flags
    kwargs.setdefault("dynamic_too", _BUILD_MODE_DYNAMIC_TOO)
    native.haskell_library(
        name = name,
        srcs = _resolve_srcs(name, srcs, all_deps, hsc_flags),
        compiler_flags = all_compiler_flags + _BUILD_MODE_HASKELL_FLAGS,
        deps = all_deps,
        **kwargs
    )

def haskell_binary(
        name,
        srcs = [],
        packages = [],
        deps = [],
        compiler_flags = [],
        fb_haskell = True,
        hsc_flags = [],
        **kwargs):
    all_deps = deps + _package_deps(packages)
    all_compiler_flags = (FB_HASKELL_EXTENSIONS + compiler_flags) if fb_haskell else compiler_flags
    kwargs.setdefault("link_style", _BUILD_MODE_LINK_STYLE)
    native.haskell_binary(
        name = name,
        srcs = _resolve_srcs(name, srcs, all_deps, hsc_flags),
        compiler_flags = all_compiler_flags + _BUILD_MODE_HASKELL_FLAGS,
        deps = all_deps,
        **kwargs
    )
