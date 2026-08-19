# A rule for running hsc2hs.
#
# The include paths passed to hsc2hs are derived entirely from `deps`: we
# reuse the same CPreprocessorInfo providers that real cxx_library() compiles
# use, so anything a C++ dependency exports via `public_include_directories`
# (or, for a haskell_prebuilt_library(), `cxx_header_dirs` - see
# mk/gen-haskell-prebuilt.py) is picked up automatically, with no hardcoded
# paths or GHC version numbers here.

load("@prelude//cxx:preprocessor.bzl", "cxx_inherited_preprocessor_infos", "cxx_merge_cpreprocessors")

# hsc2hs understands "-Idir" natively (and uses it for its own dependency
# scanning), but anything else meant for the C compiler - e.g. "-isystem
# dir" - has to be passed through via one "-C" per token. Reconstruct that
# from the raw CPreprocessor records rather than the projected `cmd_args`,
# since we need to tell "-I..." apart from everything else.
def _hsc2hs_include_args(pp_info):
    args = []
    for records in pp_info.set.traverse():
        for record in records:
            for d in record.include_dirs:
                args.append(cmd_args(d, format = "-I{}"))
            system_dirs = record.system_include_dirs.include_dirs if record.system_include_dirs else []
            for d in system_dirs:
                args.extend(["-C", "-isystem", "-C", d])
            skip = False
            for i, a in enumerate(record.args.args):
                if skip:
                    skip = False
                elif a == "-isystem" and i + 1 < len(record.args.args):
                    args.extend(["-C", "-isystem", "-C", record.args.args[i + 1]])
                    skip = True
    return args

def _hsc2hs_impl(ctx: AnalysisContext) -> list[Provider]:
    out = ctx.actions.declare_output(ctx.attrs.out)

    pp_infos = cxx_inherited_preprocessor_infos(ctx.attrs.deps)
    merged = cxx_merge_cpreprocessors(ctx.actions, [], pp_infos)

    cmd = cmd_args(
        "hsc2hs-9.4.8",
        "--cc=clang++",
        "-C",
        "-std=c++20",
        # cpp/HsStructDefines.h defines HS_STRUCT as `struct` (public by
        # default) under __HSC2HS__ and `class` (private by default)
        # otherwise, so the hsc2hs pass can see the fields it peeks/pokes
        # without a blanket -fno-access-control.
        "-C",
        "-D__HSC2HS__=1",
        # The hsc file's own package dir, so `#include "foo.h"`/`<foo.h>`
        # against a local (non-exported) header resolves, same as it would
        # when compiling a sibling cxx_library() source in this package.
        "-I" + ctx.label.package,
        _hsc2hs_include_args(merged),
        "-o",
        out.as_output(),
        ctx.attrs.hsc_file,
    )
    ctx.actions.run(cmd, category = "hsc2hs")

    return [DefaultInfo(default_output = out)]

# Runs hsc2hs on `hsc_file`, producing `out`. `deps` is used purely to
# collect C/C++ include paths (via CPreprocessorInfo); it doesn't need to be
# (and usually isn't) the same as the consuming haskell_library()'s deps.
hsc2hs = rule(
    impl = _hsc2hs_impl,
    attrs = {
        "deps": attrs.list(attrs.dep(), default = []),
        "hsc_file": attrs.source(),
        "out": attrs.string(),
    },
)
