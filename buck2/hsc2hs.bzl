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
#
# `exported_headers`'s own generated header-symlink-tree "-I" flag needs no
# special handling here despite not coming from a plain `include_directories`
# attr: `cxx_exported_preprocessor_info()`'s own `get_exported_preprocessor_
# args()` (buck2/prelude/cxx/preprocessor.bzl) bakes it directly into the
# same `CPreprocessorArgs.args` list ordinary `compiler_flags`/`exported_
# preprocessor_flags` live in - so it's already covered by the plain "args"
# projection `_hsc2hs_include_args` below uses, with no separate
# reconstruction needed (confirmed directly: removing an earlier, more
# manual symlinked-headers-tree workaround here made no difference to a
# target consuming glean/rts's own `exported_headers`).

load("@prelude//cxx:cxx_context.bzl", "get_cxx_toolchain_info")
load("@prelude//cxx:preprocessor.bzl", "cxx_inherited_preprocessor_infos", "cxx_merge_cpreprocessors")
load("@prelude//decls/toolchains_common.bzl", "toolchains_common")
load("@prelude//haskell:toolchain.bzl", "HaskellToolchainInfo")

# Every C-compiler-bound flag - "-Idir"/"-isystem dir" included -
# reaches the underlying C compiler via hsc2hs's own `--cflag=FLAG`
# (`--help`: "flag to pass to the C compiler" - the long form of `-C
# FLAG`, but critically a *single* token rather than two, unlike `-C`
# itself). `--help` also documents plain "-I DIR" as being just "passed
# to the C compiler" too - the exact same thing - so there's no
# behavioural reason to special-case "-I"/"-isystem" separately from
# anything else a CPreprocessor record contains.
#
# Using the single-token `--cflag=` form (rather than the two-token
# `-C`) is what makes it possible to use `CPreprocessorTSet`'s own
# *official* `args_projections` (`include_dirs`/`args` -
# buck2/prelude/cxx/preprocessor.bzl - the exact pair a real
# cxx_compile action itself combines for its full preprocessor command
# line, see buck2/prelude/cxx/compile.bzl) directly via `cmd_args(...,
# format = "--cflag={}")`, instead of hand-walking `pp_info.set.
# traverse()`'s raw per-record fields ourselves - confirmed directly
# that `format` applies *per projected element*, not once to the whole
# projection concatenated together. (A two-token `-C FLAG` couldn't use
# this: `pp_info.set.project_as_args(...)` returns an opaque
# `TransitiveSetArgsProjection` with no Starlark-level way to interleave
# a literal "-C" before each element - confirmed directly, too:
# `for a in pp_info.set.project_as_args("args")` fails analysis with
# `Operation (iter) not supported on type 'TransitiveSetArgsProjection'`,
# and every other prelude use of `project_as_args()` matches that -
# always appended straight into a `cmd_args`, never iterated.)
#
# This also transparently covers `external_pkgconfig_library()`'s own
# `exported_preprocessor_flags` (buck2/prelude/third-party/pkgconfig.bzl
# - used by e.g. glean/rts's `fmt` dep): a single opaque `@argsfile`
# token whose real content (`pkg-config --cflags`'s actual output)
# doesn't exist until build time. `--cflag=@argsfile` still works
# without buck2 ever needing to know what's inside it - clang/gcc both
# expand a bare `@file` response-file argument generically, wherever it
# appears on their own command line. A previous, more manual version of
# this function had no case for that opaque token at all and silently
# dropped it - invisible whenever pkg-config's own cflags output happens
# to be empty (true for a system-default-installed package, e.g. `fmt`
# via the system package manager), but a real, silent loss of a needed
# `-I` wherever pkg-config *does* report one - confirmed as the cause of
# a `hsc2hs`-stage `'fmt/core.h' file not found` that only ever
# reproduced in CI, where `fmt` is built by getdeps into a non-default
# prefix.
def _hsc2hs_include_args(pp_info):
    return [
        cmd_args(pp_info.set.project_as_args("include_dirs"), format = "--cflag={}"),
        cmd_args(pp_info.set.project_as_args("args"), format = "--cflag={}"),
    ]

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
        "-I" + ("." if ctx.label.package == "" else ctx.label.package),
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
        "extra_flags": attrs.list(attrs.string(), default = []),
        "hsc_file": attrs.source(),
        "out": attrs.string(),
        "_cxx_toolchain": toolchains_common.cxx(),
        "_haskell_toolchain": toolchains_common.haskell(),
    },
)
