load("@prelude//haskell:toolchain.bzl", "HaskellPlatformInfo", "HaskellToolchainInfo")

def _ghc948_toolchain_impl(ctx):
    return [
        DefaultInfo(),
        HaskellToolchainInfo(
            # Use GHC 9.4.8 to match the cabal build (pinned via stackage-21.21.config).
            compiler = "ghc-9.4.8",
            packager = "ghc-pkg-9.4.8",
            linker = "ghc-9.4.8",
            haddock = "haddock",
            compiler_flags = [],
            linker_flags = [],
        ),
        HaskellPlatformInfo(name = host_info().arch),
    ]

ghc948_toolchain = rule(
    impl = _ghc948_toolchain_impl,
    attrs = {},
    is_toolchain_rule = True,
)
