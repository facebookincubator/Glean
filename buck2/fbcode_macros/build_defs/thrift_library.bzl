# Shim for @fbcode_macros//build_defs:thrift_library.bzl, so gen-schema's
# checked-in-but-unmodified glean/schema/thrift/BUCK loads directly under
# this migration's own buck2 setup instead of needing to be hand-rewritten.
# See buck2.md's "gen-schema" entry.
#
# This migration only ever builds the hs2 (Haskell) backend, so every other
# `languages` entry (py/py3/rust/cpp2/java-swift) and every option specific
# to them (thrift_rust_options, thrift_cpp2_options, ...) is silently
# ignored - accepted only via **_kwargs so the real file loads unmodified.
# Aliased: this file defines its own `thrift_library()` (matching
# @fbcode_macros's API, for the unmodified gen-schema BUCK files below to
# load) - genuinely unrelated to buck2/thrift.bzl's own `thrift_library()`/
# `thrift_srcs()` (see buck2.md's "thrift_library() unification" and
# "propagate to Glean" entries), which just happens to share the name.
# Aliasing avoids the clash.
load("@root//buck2:haskell.bzl", "haskell_library")
load("@root//buck2:thrift.bzl", hs_thrift_srcs = "thrift_srcs", "thrift_srcs_export")
load(":util.bzl", "translate_deps")

def _camel(stem):
    return "".join([part[:1].upper() + part[1:] for part in stem.split("_")])

def thrift_library(
        name,
        thrift_srcs,
        deps = [],
        hs2_deps = [],
        hs_includes = [],
        **_kwargs):
    if len(thrift_srcs) != 1:
        fail("thrift_library({}): expected exactly one thrift_srcs entry, got {}".format(name, thrift_srcs))
    thrift_file = thrift_srcs.keys()[0]
    stem = thrift_file[:-len(".thrift")] if thrift_file.endswith(".thrift") else thrift_file

    # gen-schema doesn't declare an explicit `namespace hs2 ...` in these
    # files (just `namespace hs Glean.Schema`), so the hs2 backend derives
    # the module's own name segment from the .thrift file's basename -
    # confirmed against all 83 real gen-hs2 output dirs with zero
    # mismatches (see buck2.md).
    out = "Glean/Schema/{}/Types.hs".format(_camel(stem))

    # gen-schema's own `deps` already names exactly the other thrift_library()
    # targets this file's `include` statements need (schema .thrift files
    # `include` each other extensively) - translate_deps() collapses the
    # handful of Meta-internal aliases this migration doesn't build
    # separately (thrift/annotation's per-language targets, glean/if's
    # "-hs2"/no-suffix variants) onto the one real target each maps to, and
    # every such real target is itself built via this same thrift_srcs()
    # (uniformly, whether it's another gen-schema target or a hand-written
    # one like thrift/annotation), so its own "-thrift" collector always
    # exists. `hs2_deps` (angle/typed - no thrift component) stay Haskell-
    # only, same as before.
    thrift_deps = [d + "-thrift" for d in translate_deps(deps)]

    # Almost every gen-schema .thrift file `hs_include`s a same-named,
    # hand-written sibling .hs (builtin.thrift -> builtin_include.hs) -
    # thrift-compiler resolves that the same way as any other `include`
    # (against the assembled directory, by its full repo-root-relative
    # path), so it needs the same `deps` treatment as a real .thrift file.
    if hs_includes:
        thrift_srcs_export(
            name = name + "-hs-includes",
            srcs = {native.package_name() + "/" + f: f for f in hs_includes},
        )
        thrift_deps.append(":" + name + "-hs-includes")

    haskell_library(
        name = name,
        srcs = hs_thrift_srcs(
            name = name,
            thrift_files = {thrift_file: [out]},
            deps = thrift_deps,
            full_paths = True,
        ),
        deps = translate_deps(deps + hs2_deps) + [
            # Not in any deps/hs2_deps list gen-schema emits, but the
            # generated Types.hs genuinely imports Thrift.Binary.Parser/
            # Thrift.CodegenTypesOnly directly - see buck2.md.
            "@hsthrift//lib:thrift-lib",
        ],
        visibility = ["PUBLIC"],
    )
