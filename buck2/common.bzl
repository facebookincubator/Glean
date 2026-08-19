# Wrappers around the native haskell_library()/haskell_binary() rules.
#
# These handle:
#   - package deps: `packages = ["text", ...]` instead of explicit
#     `"//third-party/haskell:text"` entries in `deps`.
#   - a standard set of packages (base, rts) added to every target.
#   - the fb-haskell extension set (from the `fb-haskell` common stanza in
#     glean.cabal.in) enabled by default, so individual rules don't need to
#     repeat it.
#   - hsc2hs: any `.hsc` file in `srcs` is automatically preprocessed. The
#     include path passed to hsc2hs is the target's own package directory
#     plus the package directory of each dep; anything else (e.g. a
#     dependency's headers that live under a subdirectory, or one that isn't
#     a direct dep) can be added via `hsc_includes`.

load("//mk:hsc2hs.bzl", "hsc2hs_genrule")

# Packages implicitly needed by every Haskell target.
AUTO_PACKAGES = ["base", "rts"]

# Extensions enabled by the `fb-haskell` common stanza in glean.cabal.in.
FB_HASKELL_EXTENSIONS = [
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

def _dep_package_dir(dep):
    # "//foo/bar:baz" -> "foo/bar"; deps not in this repo (there are none
    # today) or relative deps (":baz") are skipped.
    if not dep.startswith("//") or ":" not in dep:
        return None
    return dep[2:dep.index(":")]

def _hsc_include_dirs(deps, hsc_includes):
    dirs = {package_name(): None}
    for d in deps:
        pkg = _dep_package_dir(d)
        if pkg:
            dirs[pkg] = None
    for d in hsc_includes:
        dirs[d] = None
    return dirs.keys()

def _resolve_srcs(name, srcs, deps, hsc_includes):
    includes = None
    resolved = []
    for src in srcs:
        if not src.endswith(".hsc"):
            resolved.append(src)
            continue
        if includes == None:
            includes = _hsc_include_dirs(deps, hsc_includes)
        out = src[:-len(".hsc")] + ".hs"
        rule_name = name + "-hsc-" + out.replace("/", "_")
        hsc2hs_genrule(
            name = rule_name,
            hsc_file = src,
            out = out,
            includes = includes,
        )
        resolved.append(":" + rule_name)
    return resolved

def haskell_library(
        name,
        srcs = [],
        packages = [],
        deps = [],
        compiler_flags = [],
        hsc_includes = [],
        **kwargs):
    native.haskell_library(
        name = name,
        srcs = _resolve_srcs(name, srcs, deps, hsc_includes),
        compiler_flags = FB_HASKELL_EXTENSIONS + compiler_flags,
        deps = deps + _package_deps(packages),
        **kwargs
    )

def haskell_binary(
        name,
        srcs = [],
        packages = [],
        deps = [],
        compiler_flags = [],
        hsc_includes = [],
        **kwargs):
    native.haskell_binary(
        name = name,
        srcs = _resolve_srcs(name, srcs, deps, hsc_includes),
        compiler_flags = FB_HASKELL_EXTENSIONS + compiler_flags,
        deps = deps + _package_deps(packages),
        **kwargs
    )
