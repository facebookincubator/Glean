# Helper macros for running hsc2hs in buck2 genrules.
#
# Note: $() is reserved for buck2 macro syntax in genrule cmd strings,
# so we use backtick command substitution. Inner backticks are escaped as \`.
# Plain $VAR references pass through to bash unchanged.

_HSC2HS_SETUP = (
    "ROOT=`d=\\`realpath .\\`; while [ ! -f \"$d/.buckconfig\" ]; do d=\\`dirname \"$d\"\\`; done; echo \"$d\"`" +
    " && GHC_RTS_INC=`ghc-pkg-9.4.8 field rts include-dirs 2>/dev/null | sed 's/include-dirs: //'`" +
    " && UTIL=$ROOT/hsthrift/common/util" +
    " && FOLLY=$ROOT/hsthrift/folly-clib/folly && "
)

# Include flags for hsc2hs: util headers, folly headers, GHC RTS headers.
# Optionally add extra include dirs via extra_includes (space-separated -I flags).
def hsc2hs_genrule(name, hsc_file, out, extra_includes = ""):
    flags = (
        "--cc=clang++ -C -std=c++20 -C -fno-access-control" +
        " -I \"$UTIL\" -I \"$FOLLY\" -I \"$FOLLY/_build\" -I \"$GHC_RTS_INC\"" +
        (" " + extra_includes if extra_includes else "")
    )
    native.genrule(
        name = name,
        srcs = [hsc_file],
        out = out,
        cmd = _HSC2HS_SETUP + "hsc2hs-9.4.8 " + flags + " -o $OUT $SRCDIR/" + hsc_file,
    )
