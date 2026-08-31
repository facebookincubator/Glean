# Runs gen-schema (glean/schema/gen:gen-schema) over glean/schema/source/
# *.angle, producing the .thrift/.hs/.h it generates. Mirrors the Makefile's
# own `gen-schema` target (`--dir glean/schema/source --thrift glean/schema
# --hs glean/schema --cpp glean/lang/clang/schema.h`), but into a buck2
# output directory instead of rsync-ing over the checked-in tree.
#
# gen-schema resolves --dir/--thrift/--hs/--cpp as plain relative paths
# against its own cwd (see Main.hs: `install_dir </> ... </> output`), not
# against any buck2 source artifact, so - like thrift_compile()'s
# `$(pwd)`-routed main input (buck2/thrift.bzl) - this relies on the
# action's local-execution cwd being the real repo root, with `srcs` passed
# only as `hidden` inputs to give buck2 the real dependency edge for
# caching/invalidation.
def _gen_schema_impl(ctx: AnalysisContext) -> list[Provider]:
    out = ctx.actions.declare_output(ctx.attrs.out, dir = True)
    cmd = cmd_args(
        ctx.attrs.gen_schema[RunInfo],
        "--install_dir",
        out.as_output(),
        "--dir",
        "glean/schema/source",
        "--thrift",
        "glean/schema",
        "--hs",
        "glean/schema",
        "--cpp",
        "glean/lang/clang/schema.h",
        hidden = ctx.attrs.srcs,
    )
    ctx.actions.run(cmd, category = "gen_schema", local_only = True)
    return [DefaultInfo(default_output = out)]

gen_schema = rule(
    impl = _gen_schema_impl,
    attrs = {
        "gen_schema": attrs.exec_dep(providers = [RunInfo], default = "//glean/schema/gen:gen-schema"),
        "out": attrs.string(default = "out"),
        "srcs": attrs.source(default = "//glean/schema/source:srcs"),
    },
)
