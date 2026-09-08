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
    },
    is_toolchain_rule = True,
)

