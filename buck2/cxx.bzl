# A thin wrapper around the native cxx_library() rule. Unlike
# buck2/haskell.bzl's wrappers, this doesn't need to do much - cxx_library()
# already has everything this migration needs (packages, includes, link
# flags are all plain native attrs) - except a few things worth
# centralizing instead of repeating in every cxx_library() call site:
#   - build-mode-dependent optimisation flags (see buck2.md TODO "we should
#     support different build modes"). `link_style` needs no equivalent
#     handling here: every cxx_library() in this migration leaves
#     `preferred_linkage` unset (buck2's own `Linkage("any")` default - see
#     glean/rts/BUCK's extended comment for why), which means each one
#     already automatically builds static or shared to match whatever
#     link_style the thing linking it actually requests - and that request
#     now comes from buck2/haskell.bzl's build-mode-aware `link_style` on
#     haskell_binary(), so it propagates down here for free.
#   - `-march=haswell`, matching `common fb-cpp`'s `if arch(x86_64)
#     cxx-options: -march=haswell` (glean.cabal.in) - gated on the actual
#     target CPU via prelude//cpu:x86_64, not hardcoded, so this stays
#     correct if this migration ever targets something else. Applies to
#     every cxx_library() here regardless of language, matching how
#     lmdb-clib.cabal's own C build (a separate package outside
#     glean.cabal.in entirely) gets it too, via mk/cxx-make.mk's unrelated,
#     even more blanket `CFLAGS += -march=haswell`.
#   - `-std=c++20`, matching `common fb-cpp`'s own first `cxx-options`.
#     Unlike `-march=haswell` this only makes sense for C++ - gcc hard
#     errors on `-std=c++20` against a `.c` file ("not allowed with 'C'"),
#     not just warns - so it's opt-out (`cxx_std = False`) rather than
#     unconditional, for lmdb-clib's genuine C sources.
_BUILD_MODE_CXX_FLAGS = select({
    "root//buck2/constraints:opt": ["-O3"],
    "DEFAULT": [],
})

_HASWELL_FLAGS = select({
    "prelude//cpu:x86_64": ["-march=haswell"],
    "DEFAULT": [],
})

def cxx_library(name, compiler_flags = [], cxx_std = True, **kwargs):
    std_flags = ["-std=c++20"] if cxx_std else []
    native.cxx_library(
        name = name,
        compiler_flags = std_flags + compiler_flags + _BUILD_MODE_CXX_FLAGS + _HASWELL_FLAGS,
        **kwargs
    )

# Same centralized flags for cxx_binary() - so far only glean-clang's
# `clang-index` (a genuine plain C++ executable, not Haskell-with-C++-
# sources) needs this, but the flags themselves are identical to
# cxx_library()'s, so it's a duplicate of this wrapper rather than a
# hand-rolled select() at the one call site.
def cxx_binary(name, compiler_flags = [], cxx_std = True, **kwargs):
    std_flags = ["-std=c++20"] if cxx_std else []
    native.cxx_binary(
        name = name,
        compiler_flags = std_flags + compiler_flags + _BUILD_MODE_CXX_FLAGS + _HASWELL_FLAGS,
        **kwargs
    )
