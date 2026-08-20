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
]

def _package_deps(packages):
    all_pkgs = {p: None for p in (AUTO_PACKAGES + packages)}
    return [("//third-party/haskell:" + p) for p in sorted(all_pkgs.keys())]

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

def _resolve_src(name, path, src, deps):
    # `path` is the module-derived path srcs is keyed by (e.g. what its
    # module name maps to); `src` is the actual file, which may differ from
    # `path` for a source living outside its module's directory layout (see
    # the dict form of `srcs`, below).
    out = hs_module_path(path)
    if src.endswith(".hsc"):
        rule_name = name + "-hsc-" + out.replace("/", "_")
        hsc2hs(name = rule_name, hsc_file = src, out = out, deps = deps)
        return ":" + rule_name
    elif src.endswith(".x"):
        rule_name = name + "-alex-" + out.replace("/", "_")
        alex(name = rule_name, src = src, out = out)
        return ":" + rule_name
    elif src.endswith(".y"):
        rule_name = name + "-happy-" + out.replace("/", "_")
        happy(name = rule_name, src = src, out = out)
        return ":" + rule_name
    else:
        return src

# `srcs` is usually a list, where each file's own path (relative to this
# BUCK package) determines its module name. A dict `{modulePath: file}` is
# also accepted for the rare case where a source doesn't live at the path
# its module name implies (e.g. a shared `plugins/` directory holding
# modules that belong under the main package's namespace).
def _resolve_srcs(name, srcs, deps):
    if type(srcs) == type({}):
        return {path: _resolve_src(name, path, src, deps) for path, src in srcs.items()}
    return [_resolve_src(name, src, src, deps) for src in srcs]

def haskell_library(
        name,
        srcs = [],
        packages = [],
        deps = [],
        compiler_flags = [],
        fb_haskell = True,
        **kwargs):
    all_deps = deps + _package_deps(packages)
    native.haskell_library(
        name = name,
        srcs = _resolve_srcs(name, srcs, all_deps),
        compiler_flags = (FB_HASKELL_EXTENSIONS + compiler_flags) if fb_haskell else compiler_flags,
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
        **kwargs):
    all_deps = deps + _package_deps(packages)
    native.haskell_binary(
        name = name,
        srcs = _resolve_srcs(name, srcs, all_deps),
        compiler_flags = (FB_HASKELL_EXTENSIONS + compiler_flags) if fb_haskell else compiler_flags,
        deps = all_deps,
        **kwargs
    )
