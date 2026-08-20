# A rule for running the (buck2-built) Thrift compiler over a .thrift file.
#
# Unlike hsc2hs/alex/happy, the compiler here is itself a buck2 target
# (//hsthrift/compiler:thrift-compiler), not an external frozen tool - so we
# depend on it as a normal exec_dep and let buck2 build it as part of the
# action's inputs, instead of hardcoding a path anywhere.
#
# The compiler can emit more than one output file per input (e.g. separate
# Types.hs/Service.hs/Client.hs for a .thrift file that defines a service),
# and the exact set depends on the input's contents, so this produces a
# directory rather than enumerating files. `outs`, if given, additionally
# exposes known files within that directory as sub-targets (via
# Artifact.project), so a caller who already knows what a given .thrift file
# generates can reference them directly as sources - e.g. for
# `if/Foo.thrift` generating a single `gen-hs2/Foo/Types.hs`:
#
#   thrift_compile(
#       name = "gen-foo",
#       thrift_file = "if/Foo.thrift",
#       outs = ["gen-hs2/Foo/Types.hs"],
#   )
#   haskell_library(
#       srcs = {"Foo/Types.hs": ":gen-foo[gen-hs2/Foo/Types.hs]"},
#       ...
#   )

load("//buck2:haskell.bzl", "haskell_binary", "haskell_library", "hs_module_path")

# thrift-compiler resolves *every* relative filename it's given - the main
# input included, not just `include` statements inside it - against
# --include-dir (see Thrift.Compiler.parseThriftFileE: `baseDir </> path`,
# and FilePath.(</>) only ignores baseDir when `path` is already absolute).
# A buck2 source artifact renders as a path already relative to the repo
# root, so passing it straight through alongside a relative --include-dir
# gets that dir prepended twice. Route the main input through `$(pwd)` at
# runtime (the action's cwd is the repo root) so it's genuinely absolute -
# (</>) then ignores --include-dir for it, while `include` statements
# inside the file (plain relative text baked into the source) still
# resolve correctly against the relative --include-dir as before.
_RUN_SCRIPT = 'exec "$1" "${@:4}" "$(pwd)/$2" -o "$3"'

def _thrift_compile_impl(ctx: AnalysisContext) -> list[Provider]:
    out = ctx.actions.declare_output(ctx.attrs.out, dir = True)
    cmd = cmd_args(
        "bash",
        "-c",
        _RUN_SCRIPT,
        "bash",
        ctx.attrs.compiler[RunInfo],
        ctx.attrs.thrift_file,
        out.as_output(),
        # Every consumer here wants Haskell output, so --hs is unconditional
        # rather than something every caller has to remember to pass.
        "--hs",
        ctx.attrs.flags,
    )
    ctx.actions.run(cmd, category = "thrift_compile")

    sub_targets = {
        rel_path: [DefaultInfo(default_output = out.project(rel_path))]
        for rel_path in ctx.attrs.outs
    }
    return [DefaultInfo(default_output = out, sub_targets = sub_targets)]

# `flags` are extra thrift-compiler flags beyond --hs (which is always
# added - see _thrift_compile_impl) - e.g. ["--use-int"], or ["-I", DIR] to
# resolve `include` statements in the file. `out` names the output
# directory; the compiler writes into `out/gen-hs2/...` (or wherever
# `--gen-prefix` in `flags` says) by default.
thrift_compile = rule(
    impl = _thrift_compile_impl,
    attrs = {
        "compiler": attrs.exec_dep(providers = [RunInfo], default = "//hsthrift/compiler:thrift-compiler"),
        "flags": attrs.list(attrs.string(), default = []),
        "out": attrs.string(default = "gen"),
        "outs": attrs.list(attrs.string(), default = []),
        "thrift_file": attrs.source(),
    },
)

def _thrift_stem(thrift_file):
    # thrift_file may be a plain path ("if/Foo.thrift") or a target label
    # referencing an exported file in another package
    # ("//other/pkg:Foo.thrift") - strip both a leading "//pkg:" and any
    # directory components to get a name usable as (part of) a target name.
    base = thrift_file.split(":")[-1].split("/")[-1]
    return base[:-len(".thrift")] if base.endswith(".thrift") else base

# haskell_library()/haskell_binary(), but their .thrift dependencies (and
# the thrift_compile() targets for them) are declared inline instead of by
# hand. `thrift_files` maps each .thrift file to the list of Haskell files
# it generates (module paths, same as any other srcs entry - not
# gen-hs2-prefixed):
#
#   thrift_haskell_library(
#       name = "foo",
#       thrift_files = {
#           "if/Foo.thrift": ["Foo/Types.hs", "Foo/Service.hs"],
#       },
#       srcs = ["Handwritten.hs"],
#       ...
#   )
#
# is equivalent to declaring a thrift_compile() per .thrift file (with
# outs = ["gen-hs2/" + f for f in ...]) and adding
# {"Foo/Types.hs": ":<gen-target>[gen-hs2/Foo/Types.hs]", ...} to srcs by
# hand, as hsthrift/lib/BUCK originally did for gen-rpc-options/
# gen-application-exception.
#
# thrift_flags applies to every entry in thrift_files (--hs is automatic,
# don't include it); a .thrift file needing different flags entirely (e.g.
# a different -I - thrift-compiler rejects a repeated -I, so this can't
# just be appended to thrift_flags) can replace them via
# thrift_file_flags = {"if/Foo.thrift": ["--use-int", "-I", "other/dir"]}.
def _thrift_srcs(name, thrift_files, thrift_flags, thrift_file_flags, srcs):
    # A plain srcs list relies on haskell_library()/haskell_binary()
    # deriving each entry's module path from the file itself (stripping
    # .hsc/.x/.y as needed); replicate that here so merging in the
    # generated entries below doesn't change what a caller's existing
    # (non-thrift) srcs list resolves to.
    all_srcs = {hs_module_path(s): s for s in srcs} if type(srcs) != type({}) else dict(srcs)

    for thrift_file, outs in thrift_files.items():
        gen_name = name + "-thrift-" + _thrift_stem(thrift_file)
        gen_outs = ["gen-hs2/" + o for o in outs]
        thrift_compile(
            name = gen_name,
            thrift_file = thrift_file,
            flags = thrift_file_flags.get(thrift_file, thrift_flags),
            outs = gen_outs,
        )
        for out, gen_out in zip(outs, gen_outs):
            all_srcs[out] = ":{}[{}]".format(gen_name, gen_out)

    return all_srcs

def thrift_haskell_library(
        name,
        thrift_files = {},
        thrift_flags = [],
        thrift_file_flags = {},
        srcs = [],
        **kwargs):
    all_srcs = _thrift_srcs(name, thrift_files, thrift_flags, thrift_file_flags, srcs)
    haskell_library(name = name, srcs = all_srcs, **kwargs)

def thrift_haskell_binary(
        name,
        thrift_files = {},
        thrift_flags = [],
        thrift_file_flags = {},
        srcs = [],
        **kwargs):
    all_srcs = _thrift_srcs(name, thrift_files, thrift_flags, thrift_file_flags, srcs)
    haskell_binary(name = name, srcs = all_srcs, **kwargs)
