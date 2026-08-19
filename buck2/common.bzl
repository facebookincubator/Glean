# Wrappers around the native haskell_library()/haskell_binary() rules.
#
# These handle:
#   - package deps: `packages = ["text", ...]` instead of explicit
#     `"//third-party/haskell:text"` entries in `deps`.
#   - a standard set of packages (base, rts) added to every target.
#   - the fb-haskell extension set (from the `fb-haskell` common stanza in
#     glean.cabal.in) enabled by default, so individual rules don't need to
#     repeat it.
#   - hsc2hs: any `.hsc` file in `srcs` is automatically preprocessed, with
#     include paths derived from `deps` (see mk/hsc2hs.bzl) - so a `.hsc`
#     file that needs a C++ dependency's headers just needs that dependency
#     listed in `deps`, same as any other buck2 target.

load("//mk:hsc2hs.bzl", "hsc2hs")

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

def _resolve_srcs(name, srcs, deps):
    resolved = []
    for src in srcs:
        if not src.endswith(".hsc"):
            resolved.append(src)
            continue
        out = src[:-len(".hsc")] + ".hs"
        rule_name = name + "-hsc-" + out.replace("/", "_")
        hsc2hs(
            name = rule_name,
            hsc_file = src,
            out = out,
            deps = deps,
        )
        resolved.append(":" + rule_name)
    return resolved

def haskell_library(
        name,
        srcs = [],
        packages = [],
        deps = [],
        compiler_flags = [],
        **kwargs):
    all_deps = deps + _package_deps(packages)
    native.haskell_library(
        name = name,
        srcs = _resolve_srcs(name, srcs, all_deps),
        compiler_flags = FB_HASKELL_EXTENSIONS + compiler_flags,
        deps = all_deps,
        **kwargs
    )

def haskell_binary(
        name,
        srcs = [],
        packages = [],
        deps = [],
        compiler_flags = [],
        **kwargs):
    all_deps = deps + _package_deps(packages)
    native.haskell_binary(
        name = name,
        srcs = _resolve_srcs(name, srcs, all_deps),
        compiler_flags = FB_HASKELL_EXTENSIONS + compiler_flags,
        deps = all_deps,
        **kwargs
    )
