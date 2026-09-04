# A rule for running hsc2hs.
#
# The include paths passed to hsc2hs are derived entirely from `deps`: we
# reuse the same CPreprocessorInfo providers that real cxx_library() compiles
# use, so anything a C++ dependency exports via `public_include_directories`,
# `exported_headers` (see buck2/cxx.bzl), or (for a haskell_prebuilt_
# library()) `cxx_header_dirs` (see buck2/gen-haskell-prebuilt.py) is picked up
# automatically. The C++ compiler and the hsc2hs binary itself both come
# from the cxx/haskell toolchains (toolchains/BUCK, toolchains/haskell.bzl)
# rather than being hardcoded here.

load("@prelude//:paths.bzl", "paths")
load("@prelude//cxx:cxx_context.bzl", "get_cxx_toolchain_info")
load("@prelude//cxx:preprocessor.bzl", "cxx_inherited_preprocessor_infos", "cxx_merge_cpreprocessors")
load("@prelude//decls/toolchains_common.bzl", "toolchains_common")
load("@prelude//haskell:toolchain.bzl", "HaskellToolchainInfo")

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

# A C++ dep that exposes its headers via `headers`/`exported_headers`
# (see buck2/cxx.bzl) rather than `include_directories`/`public_include_
# directories` doesn't put anything in `record.include_dirs` at all - the
# generated symlink-tree include path lives inside the opaque `record.args`
# cmd_args instead (built by the prelude's own `get_exported_preprocessor_
# args`), which `_hsc2hs_include_args` above has no way to pick apart from
# everything else in there. Rather than reverse-engineer that cmd_args,
# rebuild an equivalent tree ourselves directly from `record.headers` (a
# plain, non-opaque `list[CHeader]` - name/namespace/artifact), which is
# exactly the same header set the real cxx_library() compile would see.
def _hsc2hs_headers_dir(ctx, pp_info):
    headers = {}
    for records in pp_info.set.traverse():
        for record in records:
            for h in record.headers:
                key = paths.join(h.namespace, h.name) if h.namespace else h.name
                headers[key] = h.artifact
    if not headers:
        return None
    return ctx.actions.symlinked_dir("hsc2hs-headers", headers)

def _hsc2hs_impl(ctx: AnalysisContext) -> list[Provider]:
    out = ctx.actions.declare_output(ctx.attrs.out)

    pp_infos = cxx_inherited_preprocessor_infos(ctx.attrs.deps)
    merged = cxx_merge_cpreprocessors(ctx.actions, [], pp_infos)

    # hsc2hs ships with GHC itself (as hsc2hs-<version>, e.g. hsc2hs-9.4.8),
    # not as a separately built/versioned tool, so there's no dedicated
    # toolchain field for it (unlike ALEX/HAPPY in third-party/haskell/
    # tools.bzl, which really are separate Cabal packages) - derive the
    # version from the haskell toolchain's own compiler name instead
    # (buck2/toolchains/BUCK sets compiler = "ghc-" + GHC_VERSION, read
    # from Cabal's own resolved plan - see buck2/gen-haskell-prebuilt.py).
    ghc_compiler = ctx.attrs._haskell_toolchain[HaskellToolchainInfo].compiler
    ghc_version = ghc_compiler[len("ghc-"):] if ghc_compiler.startswith("ghc-") else ghc_compiler
    hsc2hs_tool = "hsc2hs-" + ghc_version

    cxx_compiler = get_cxx_toolchain_info(ctx).cxx_compiler_info.compiler

    headers_dir = _hsc2hs_headers_dir(ctx, merged)

    cmd = cmd_args(
        hsc2hs_tool,
        cmd_args("--cc=", cxx_compiler, delimiter = ""),
        "-C",
        "-std=c++20",
        # cpp/HsStructDefines.h defines HS_STRUCT as `struct` (public by
        # default) under __HSC2HS__ and `class` (private by default)
        # otherwise, so the hsc2hs pass can see the fields it peeks/pokes
        # without a blanket -fno-access-control.
        "-C",
        "-D__HSC2HS__=1",
        # Extra -C-style C-compiler flags, e.g. -D for a Cabal-generated
        # MIN_VERSION_<pkg> macro a .hsc file's own CPP relies on (Cabal
        # synthesizes these from a library's build-depends via
        # cabal_macros.h; buck2 has no equivalent, so a caller needing one
        # passes it explicitly - see haskell_library()'s hsc_flags).
        ctx.attrs.extra_flags,
        # The hsc file's own package dir, so `#include "foo.h"`/`<foo.h>`
        # against a local (non-exported) header resolves, same as it would
        # when compiling a sibling cxx_library() source in this package.
        "-I" + ctx.label.package,
        _hsc2hs_include_args(merged),
        cmd_args(headers_dir, format = "-I{}") if headers_dir else [],
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
        "extra_flags": attrs.list(attrs.string(), default = []),
        "hsc_file": attrs.source(),
        "out": attrs.string(),
        "_cxx_toolchain": toolchains_common.cxx(),
        "_haskell_toolchain": toolchains_common.haskell(),
    },
)
