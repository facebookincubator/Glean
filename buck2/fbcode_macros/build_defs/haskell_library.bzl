# Shim for @fbcode_macros//build_defs:haskell_library.bzl, so gen-schema's
# checked-in-but-unmodified glean/schema/hs/BUCK loads directly under this
# migration's own buck2 setup. See buck2.md's "gen-schema" entry.
load("@root//buck2:haskell.bzl", real_haskell_library = "haskell_library")
load(":util.bzl", "translate_deps")

def haskell_library(name, srcs, deps = [], **_kwargs):
    real_haskell_library(
        name = name,
        srcs = srcs,
        deps = translate_deps(deps),
        visibility = ["PUBLIC"],
    )
