load("@prelude//haskell:toolchain.bzl", "HaskellPlatformInfo", "HaskellToolchainInfo")

def _haskell_toolchain_impl(ctx):
    return [
        DefaultInfo(),
        HaskellToolchainInfo(
            compiler = ctx.attrs.compiler,
            packager = ctx.attrs.packager,
            linker = ctx.attrs.compiler,
            haddock = ctx.attrs.haddock,
            compiler_flags = ctx.attrs.compiler_flags,
            linker_flags = ctx.attrs.linker_flags,
            compile_env = ctx.attrs.compile_env,
            dynamic_ghc = ctx.attrs.dynamic_ghc,
        ),
        HaskellPlatformInfo(name = host_info().arch),
    ]

haskell_toolchain = rule(
    impl = _haskell_toolchain_impl,
    attrs = {
        "compiler": attrs.string(default = "ghc"),
        "packager": attrs.string(default = "ghc-pkg"),
        "haddock": attrs.string(default = "haddock"),
        "compiler_flags": attrs.list(attrs.string(), default = []),
        "linker_flags": attrs.list(attrs.string(), default = []),
        "compile_env": attrs.dict(attrs.string(), attrs.string(), default = {}),
        # Whether the `compiler` above is itself dynamically linked -
        # see buck2/gen-haskell-prebuilt.py's own `_ghc_dynamic()` for
        # how this gets discovered (not assumed), and buck2/prelude/
        # haskell/haskell.bzl's own uses of `haskell_toolchain.
        # dynamic_ghc` for what it actually gates.
        "dynamic_ghc": attrs.bool(default = True),
    },
    is_toolchain_rule = True,
)
