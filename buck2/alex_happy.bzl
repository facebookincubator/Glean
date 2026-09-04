# Rules for running alex and happy.
#
# Unlike hsc2hs, alex/happy don't ship with GHC - they're separate Hackage
# packages. buck2/gen-haskell-prebuilt.py asks Cabal where it put them
# (`cabal list-bin alex`/`happy`) and freezes the answer in
# third-party/haskell/tools.bzl, the same "Cabal identifies/builds it,
# buck2 just references the frozen result" approach third-party/haskell/BUCK
# uses for library packages. Re-run that script to pick up a version bump.

load("@third-party//haskell:tools.bzl", "ALEX", "HAPPY")

def _run_tool_impl(ctx: AnalysisContext) -> list[Provider]:
    out = ctx.actions.declare_output(ctx.attrs.out)
    ctx.actions.run(
        cmd_args(ctx.attrs.tool, ctx.attrs.src, "-o", out.as_output()),
        category = ctx.attrs.category,
    )
    return [DefaultInfo(default_output = out)]

_run_tool = rule(
    impl = _run_tool_impl,
    attrs = {
        "category": attrs.string(),
        "out": attrs.string(),
        "src": attrs.source(),
        "tool": attrs.string(),
    },
)

def alex(name, src, out):
    _run_tool(name = name, src = src, out = out, tool = ALEX, category = "alex")

def happy(name, src, out):
    _run_tool(name = name, src = src, out = out, tool = HAPPY, category = "happy")
