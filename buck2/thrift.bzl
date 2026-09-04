load("//buck2:haskell.bzl", "haskell_library")

# A rule for running the (buck2-built) Thrift compiler over a .thrift file.
#
# Unlike hsc2hs/alex/happy, the compiler here is itself a buck2 target
# (//compiler:thrift-compiler), not an external frozen tool - so we
# depend on it as a normal exec_dep and let buck2 build it as part of the
# action's inputs, instead of hardcoding a path anywhere.
#
# The compiler can emit more than one output file per input (e.g. separate
# Types.hs/Service.hs/Client.hs for a .thrift file that defines a service),
# and the exact set depends on the input's contents, so it always writes
# into a directory (`gen-hs2/...` by default) rather than a fixed file.
# `outs`, if given, additionally names known files within that directory
# (as plain module paths, e.g. "Foo/Types.hs" - not gen-hs2-prefixed) and
# exposes each as its own correctly-pathed sub-target artifact (via
# ctx.actions.copy_file() - the same primitive export_file.bzl uses), so a
# caller who already knows what a .thrift file generates can put them
# directly into a haskell_library()'s plain srcs *list*:
#
#   thrift_compile(
#       name = "gen-foo",
#       thrift_file = "if/Foo.thrift",
#       outs = ["Foo/Types.hs"],
#   )
#   haskell_library(
#       srcs = [":gen-foo[Foo/Types.hs]"],
#       ...
#   )
#
# This isn't just a style choice: haskell_library() derives each module's
# name (for the package db, so cross-target `import`s of it resolve) from
# its source artifact's own path - a bare copy of the whole gen-hs2/
# directory would derive "gen.gen-hs2.Foo.Types" instead of "Foo.Types".

# thrift-compiler's default --gen-prefix; every caller in this repo relies
# on the default, so `outs` entries are resolved as GEN_PREFIX + "/" + out
# within the raw output directory.
_GEN_PREFIX = "gen-hs2"

# thrift-compiler resolves *every* relative filename it's given - the main
# INPUT included, not just `include` statements inside a .thrift file -
# against --include-dir (Thrift.Compiler.OptParse.optsIncludePath, default
# "."; see also Thrift.Compiler.parseThriftFileE: `baseDir </> path`, and
# FilePath.(</>) only ignores baseDir when `path` is already absolute).
#
# A directory named directly on the command line (a plain "-I some/dir"
# string, or a glob'd filegroup covering "everything in this package") is
# a bad fit for that: a plain path can't tell "this cell built standalone"
# apart from "this cell built nested inside another project at a different
# filesystem depth" (see buck2.md's hsthrift-cell entry), and a glob'd
# directory isn't hermetic - remote execution materializes only an
# action's *declared* inputs, not whatever real files happen to sit next
# to the main one in the source tree (which is also what made a shared
# "thrift" symlink inside both lib/ and tests/ - see thrift/BUCK at the
# cell root - actively break: Buck2 saw one input (a glob covering the
# symlink itself) wanting it to be a leaf/symlink, and another input
# (an explicit thrift_file reference reached *through* it) wanting the
# same path to be a real directory, and refused to merge the two).
#
# So instead: every file an `include` statement might ever need is reached
# through `deps` - other thrift_srcs_export()/thrift_library() targets,
# whose own .thrift files (and *their* deps, transitively - see
# ThriftSrcsInfo below) get merged into one fresh directory via
# ctx.actions.symlinked_dir() before running the compiler *from inside
# it*, relying on the "." default to do the resolving. That directory is
# a completely ordinary derived artifact, so RE ships it (and the real
# files it points at) to the worker like any other action input - nothing
# about it depends on where this cell happens to sit in a checkout.
_RUN_SCRIPT = '''
set -eu
root="$(pwd)"
compiler="$root/$1"
src_dir="$root/$2"
out="$root/$3"
name="$4"
shift 4
cd "$src_dir"
exec "$compiler" "$name" -o "$out" "$@"
'''

# Propagated by thrift_srcs_export() (and hence by thrift_library(), which
# always creates one) and consumed by thrift_compile() - a transitive set
# of {relative include path: source artifact} dicts, one per target in the
# `deps` chain, so a target only has to list its own *direct* thrift deps
# and still gets the full transitive closure of `include`able files (a
# depends on b depends on c: a's `deps = ["//:b"]` is enough, a's own
# thrift_compile() actions see c's files too, without a needing to know c
# exists). `.traverse()` yields each node's own dict; thrift_compile()
# merges them all together.
ThriftSrcsTSet = transitive_set()

ThriftSrcsInfo = provider(fields = {"tset": provider_field(typing.Any)})

def _thrift_srcs_export_impl(ctx: AnalysisContext) -> list[Provider]:
    children = [dep[ThriftSrcsInfo].tset for dep in ctx.attrs.deps]
    tset = ctx.actions.tset(ThriftSrcsTSet, value = ctx.attrs.srcs, children = children)

    # A plain merged directory of just this target's *own* direct srcs
    # (not the transitive closure - thrift_compile() does that merge
    # itself via the tset above), for anything wanting the raw files
    # directly rather than through thrift_compile() - e.g. `buck2 build`
    # on this target alone to inspect what it exports.
    out = ctx.actions.symlinked_dir("thrift-srcs", ctx.attrs.srcs)

    return [DefaultInfo(default_output = out), ThriftSrcsInfo(tset = tset)]

# Exports a set of .thrift source files (`srcs`, keyed by the relative
# path an `include` statement elsewhere would name them by) for other
# thrift_srcs_export()/thrift_library() targets to depend on - directly
# creating one of these by hand is only needed for raw sources with no
# Haskell compile of their own at this location (e.g. hsthrift/BUCK's
# thrift/annotation/*.thrift, reused - and independently compiled - by
# several different packages); thrift_library() below always creates one
# implicitly, named after itself, for its own `thrift_files`.
thrift_srcs_export = rule(
    impl = _thrift_srcs_export_impl,
    attrs = {
        "deps": attrs.list(attrs.dep(providers = [ThriftSrcsInfo]), default = []),
        "srcs": attrs.dict(attrs.string(), attrs.source(), default = {}),
    },
)

def _thrift_compile_impl(ctx: AnalysisContext) -> list[Provider]:
    out = ctx.actions.declare_output(ctx.attrs.out, dir = True)

    srcs = {}
    for dep in ctx.attrs.deps:
        for group in dep[ThriftSrcsInfo].tset.traverse():
            srcs.update(group)
    srcs[ctx.attrs.thrift_name] = ctx.attrs.thrift_file
    src_dir = ctx.actions.symlinked_dir(ctx.attrs.out + "-src", srcs)

    cmd = cmd_args(
        "bash",
        "-c",
        _RUN_SCRIPT,
        "bash",
        ctx.attrs.compiler[RunInfo],
        src_dir,
        out.as_output(),
        ctx.attrs.thrift_name,
        # Every consumer here wants Haskell output, so --hs is unconditional
        # rather than something every caller has to remember to pass.
        "--hs",
        ctx.attrs.flags,
    )
    ctx.actions.run(cmd, category = "thrift_compile")

    sub_targets = {
        clean_path: [DefaultInfo(default_output = ctx.actions.copy_file(
            clean_path,
            out.project(_GEN_PREFIX + "/" + clean_path),
        ))]
        for clean_path in ctx.attrs.outs
    }
    return [DefaultInfo(default_output = out, sub_targets = sub_targets)]

# `flags` are extra thrift-compiler flags beyond --hs (which is always
# added - see _thrift_compile_impl) - e.g. ["--use-int"]. `deps` are
# thrift_srcs_export()/thrift_library() targets providing every file this
# one's `include`/`hs_include` statements need, transitively - see
# ThriftSrcsInfo above and this file's own top comment for why that's a
# `deps` edge rather than a `-I DIR` flag or a glob'd directory.
# `thrift_name` is the relative path *this* file itself is known as
# (matching how some other file's `include` statement would name it, if
# any does - purely cosmetic otherwise). `out` names the output
# directory; the compiler writes into `out/gen-hs2/...` (or wherever
# `--gen-prefix` in `flags` says) by default.
thrift_compile = rule(
    impl = _thrift_compile_impl,
    attrs = {
        "compiler": attrs.exec_dep(providers = [RunInfo], default = "//compiler:thrift-compiler"),
        "deps": attrs.list(attrs.dep(providers = [ThriftSrcsInfo]), default = []),
        "flags": attrs.list(attrs.string(), default = []),
        "out": attrs.string(default = "gen"),
        "outs": attrs.list(attrs.string(), default = []),
        "thrift_file": attrs.source(),
        "thrift_name": attrs.string(),
    },
)

def _thrift_stem(thrift_file):
    # thrift_file may be a plain path ("if/Foo.thrift") or a target label
    # referencing an exported file in another package
    # ("//other/pkg:Foo.thrift") - strip both a leading "//pkg:" and any
    # directory components to get a name usable as (part of) a target name.
    base = thrift_file.split(":")[-1].split("/")[-1]
    return base[:-len(".thrift")] if base.endswith(".thrift") else base

def _thrift_name(thrift_file, full_paths = False):
    # The relative path thrift_file is known as inside the assembled
    # directory (see thrift_compile's own comment) - matching how some
    # other file's `include` statement would name it, so that statement
    # resolves once merged into the same directory. A cross-package label
    # ("//other/pkg:Foo.thrift") has no such path to reuse - nothing in
    # this repo ever `include`s a file referenced that way (it's always
    # *itself* the main file being compiled, via export_file), so its
    # in-sandbox name only has to be a valid one, not any particular one.
    if "//" in thrift_file or thrift_file.startswith(":"):
        return _thrift_stem(thrift_file) + ".thrift"

    # Two real, incompatible conventions exist for a plain relative path:
    # hsthrift's own .thrift files `include` each other by a path relative
    # to whichever package they live in (e.g. "if/Foo.thrift", no package
    # prefix - inherited from the Makefile era, where the compiler's cwd
    # was set to that package directly); Glean's own .thrift files
    # `include` each other by the *full* repo-root-relative path instead
    # (e.g. "glean/config/service.thrift"). full_paths picks which one a
    # given thrift_srcs()/thrift_library() caller's files use - it can't
    # be auto-detected, since both are just plain strings.
    if full_paths:
        return native.package_name() + "/" + thrift_file
    return thrift_file

def _thrift_compile_all(name, thrift_files, thrift_flags, thrift_file_flags, deps, srcs, full_paths):
    # Shared by thrift_srcs() and thrift_library() below: declares one
    # thrift_compile() per thrift_files entry (each one's own `deps`
    # pointing back at `name`'s own collector target, so every file in
    # this same call automatically sees every *other* file in it as a
    # potential `include` target, with no per-file bookkeeping needed -
    # e.g. tests/if/A.thrift including B/C/D/E.thrift, all declared in the
    # same thrift_files dict, just works), and returns the resulting srcs
    # dict for a haskell_*() target.
    all_srcs = dict(srcs) if type(srcs) == type({}) else {s: s for s in srcs}
    for thrift_file, outs in thrift_files.items():
        gen_name = name + "-thrift-" + _thrift_stem(thrift_file)
        thrift_compile(
            name = gen_name,
            thrift_file = thrift_file,
            thrift_name = _thrift_name(thrift_file, full_paths),
            flags = thrift_file_flags.get(thrift_file, thrift_flags),
            deps = deps,
            outs = outs,
        )
        for out in outs:
            all_srcs[out] = ":{}[{}]".format(gen_name, out)
    return all_srcs

# Lower-level than thrift_library() below: creates the same per-file
# thrift_compile() sub-targets and a `name + "-thrift"` collector (see
# thrift_srcs_export()) other targets' `deps` can reference, but returns
# the resulting srcs dict directly instead of also creating a
# haskell_library() - for haskell_test()/haskell_binary() callers, which
# own their own top-level target name already (thrift_library() would
# collide with it - see thrift_library()'s own comment for why its
# collector uses the *bare* name instead).
#
#   haskell_test(
#       name = "foo",
#       srcs = thrift_srcs(
#           name = "foo",
#           thrift_files = {"if/Foo.thrift": ["Foo/Types.hs"]},
#           deps = ["//:annotations"],
#       ),
#       ...
#   )
def thrift_srcs(
        name,
        thrift_files = {},
        thrift_flags = [],
        thrift_file_flags = {},
        deps = [],
        srcs = {},
        visibility = ["PUBLIC"],
        full_paths = False):
    collector = name + "-thrift"
    thrift_srcs_export(
        name = collector,
        srcs = {_thrift_name(f, full_paths): f for f in thrift_files},
        deps = deps,
        visibility = visibility,
    )
    return _thrift_compile_all(name, thrift_files, thrift_flags, thrift_file_flags, [":" + collector], srcs, full_paths)

# The usual entry point: compiles `thrift_files` (mapping each .thrift
# file to the list of Haskell files it generates - module paths, same as
# any other srcs entry, not gen-hs2-prefixed) and declares a
# `name + "-hs2"` haskell_library() from the result, plus a thrift_srcs_
# export() at the *bare* `name` other thrift_library()/thrift_srcs() call
# sites can put in their own `deps` - matching @fbcode_macros's own
# thrift_library() wrapper (buck2/fbcode_macros/build_defs/thrift_library.
# bzl), which already has this exact `deps`/`hs2_deps` split for the
# (Thrift-generated-code-only) targets gen-schema declares.
#
# `deps` are other thrift_library() targets (referenced by their own bare
# `name`) whose .thrift files these thrift_files may `include`, resolved
# transitively (see ThriftSrcsInfo) - the generated Haskell code almost
# always ends up importing from them too (e.g. a struct field typed with
# an included file's own type), so `deps` feeds the -hs2 library's own
# `deps` as well (via `d + "-hs2"`), not just the thrift_compile()
# actions. `hs2_deps` are for anything the -hs2 library needs that has no
# thrift component of its own (fb-util, containers, ...).
#
#   thrift_library(
#       name = "foo",
#       thrift_files = {"if/Foo.thrift": ["Foo/Types.hs"]},
#       deps = ["//:annotations"],
#       hs2_deps = ["//common/util:fb-util"],
#   )
#
# is equivalent to declaring a thrift_compile() per .thrift file, adding
# {"Foo/Types.hs": ":<gen-target>[Foo/Types.hs]", ...} to srcs by hand,
# and writing the haskell_library() out explicitly, as hsthrift/lib/BUCK
# originally did for gen-rpc-options/gen-application-exception.
def thrift_library(
        name,
        thrift_files = {},
        thrift_flags = [],
        thrift_file_flags = {},
        deps = [],
        hs2_deps = [],
        srcs = {},
        visibility = ["PUBLIC"],
        full_paths = False,
        **haskell_kwargs):
    thrift_srcs_export(
        name = name,
        srcs = {_thrift_name(f, full_paths): f for f in thrift_files},
        deps = deps,
        visibility = visibility,
    )
    all_srcs = _thrift_compile_all(name, thrift_files, thrift_flags, thrift_file_flags, [":" + name], srcs, full_paths)
    haskell_library(
        name = name + "-hs2",
        srcs = all_srcs,
        deps = [d + "-hs2" for d in deps] + hs2_deps,
        visibility = visibility,
        **haskell_kwargs
    )
