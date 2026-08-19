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

def _thrift_compile_impl(ctx: AnalysisContext) -> list[Provider]:
    out = ctx.actions.declare_output(ctx.attrs.out, dir = True)
    cmd = cmd_args(
        ctx.attrs.compiler[RunInfo],
        ctx.attrs.flags,
        ctx.attrs.thrift_file,
        "-o",
        out.as_output(),
    )
    ctx.actions.run(cmd, category = "thrift_compile")

    sub_targets = {
        rel_path: [DefaultInfo(default_output = out.project(rel_path))]
        for rel_path in ctx.attrs.outs
    }
    return [DefaultInfo(default_output = out, sub_targets = sub_targets)]

# `flags` defaults to ["--hs"] (generate Haskell); pass e.g. ["--hs",
# "--use-int"] to match a Makefile invocation that adds extra flags. `out`
# names the output directory; the compiler writes into `out/gen-hs2/...`
# (or wherever `--gen-prefix` in `flags` says) by default.
thrift_compile = rule(
    impl = _thrift_compile_impl,
    attrs = {
        "compiler": attrs.exec_dep(providers = [RunInfo], default = "//hsthrift/compiler:thrift-compiler"),
        "flags": attrs.list(attrs.string(), default = ["--hs"]),
        "out": attrs.string(default = "gen"),
        "outs": attrs.list(attrs.string(), default = []),
        "thrift_file": attrs.source(),
    },
)
