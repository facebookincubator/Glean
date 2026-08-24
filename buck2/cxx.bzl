# A thin wrapper around the native cxx_library() rule. Unlike
# buck2/haskell.bzl's wrappers, this doesn't need to do much - cxx_library()
# already has everything this migration needs (packages, includes, link
# flags are all plain native attrs) - except one thing worth centralizing:
# build-mode-dependent optimisation flags (see buck2.md TODO "we should
# support different build modes"). `link_style` needs no equivalent
# handling here: every cxx_library() in this migration leaves
# `preferred_linkage` unset (buck2's own `Linkage("any")` default - see
# glean/rts/BUCK's extended comment for why), which means each one already
# automatically builds static or shared to match whatever link_style the
# thing linking it actually requests - and that request now comes from
# buck2/haskell.bzl's build-mode-aware `link_style` on haskell_binary(), so
# it propagates down here for free.
_BUILD_MODE_CXX_FLAGS = select({
    "root//buck2/constraints:opt": ["-O3"],
    "DEFAULT": [],
})

def cxx_library(name, compiler_flags = [], **kwargs):
    native.cxx_library(
        name = name,
        compiler_flags = compiler_flags + _BUILD_MODE_CXX_FLAGS,
        **kwargs
    )
