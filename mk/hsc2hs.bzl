# Helper macro for running hsc2hs in buck2 genrules.
#
# Note: $() is reserved for buck2 macro syntax in genrule cmd strings,
# so we use backtick command substitution. Inner backticks are escaped as \`.
# Plain $VAR references pass through to bash unchanged.

_FIND_ROOT = (
    "ROOT=`d=\\`realpath .\\`; while [ ! -f \"$d/.buckconfig\" ]; do d=\\`dirname \"$d\"\\`; done; echo \"$d\"`"
)

# hsc2hs needs to see Rts.h. Resolve it via the checked-in third-party/haskell
# GHC symlink (see mk/gen-haskell-prebuilt.py) instead of shelling out to
# ghc-pkg, so it doesn't depend on a specific ghc-pkg binary being on PATH.
_GHC_VERSION = "9.4.8"
_RTS_VERSION = "1.0.2"
_RTS_INCLUDE_DIR = (
    "third-party/haskell/ghc-{v}/lib/ghc-{v}/lib/x86_64-linux-ghc-{v}/rts-{r}/include"
        .format(v = _GHC_VERSION, r = _RTS_VERSION)
)

# Runs hsc2hs on `hsc_file`, producing `out`. `includes` is a list of
# repo-root-relative directories to search for headers (in addition to the
# GHC RTS include dir, which is always added).
def hsc2hs_genrule(name, hsc_file, out, includes = []):
    all_includes = includes + [_RTS_INCLUDE_DIR]
    flags = (
        "--cc=clang++ -C -std=c++20 -C -fno-access-control " +
        " ".join(["-I \"$ROOT/%s\"" % i for i in all_includes])
    )
    native.genrule(
        name = name,
        srcs = [hsc_file],
        out = out,
        cmd = _FIND_ROOT + " && hsc2hs-9.4.8 " + flags + " -o $OUT $SRCDIR/" + hsc_file,
    )
