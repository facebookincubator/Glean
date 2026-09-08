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
#   - `-Wno-nullability-completeness -fno-omit-frame-pointer` (`common
#     fb-cpp`'s own unconditional flags) - the former is a Clang-only
#     diagnostic (silently ignored by GCC, so unconditional is safe either
#     way), the latter keeps stack traces/profiling working.
#   - `-fcoroutines` unless `flag(clang)` (`common fb-cpp`: `if !flag(clang)
#     cxx-options: -fcoroutines`) - GCC needs it to enable C++20 coroutines,
#     Clang doesn't (and may not recognise it) - see buck2/constraints/BUCK's
#     own `clang` value for why this doesn't itself switch compilers.
#   - `-fsanitize=address` if `flag(asan)` (`common fb-cpp`: `if flag(asan)
#     cxx-options: -fsanitize=address`) - selected via `-m //buck2/
#     constraints:asan`.
#   - `-O3 -DNDEBUG` in `opt` mode, *unless* asan is also active, in which
#     case `-g` instead - matching `common fb-cpp`'s own `if flag(opt) &&
#     !flag(asan) cxx-options: -O3 -DNDEBUG else cxx-options: -g` exactly
#     (asan diagnostics need debug info and are meaningless optimised away,
#     so opt+asan intentionally doesn't optimise).
_ASAN_CXX_FLAGS = select({
    "root//buck2/constraints:asan": ["-fsanitize=address"],
    "DEFAULT": [],
})

_COROUTINES_FLAGS = select({
    "root//buck2/constraints:clang": [],
    "DEFAULT": ["-fcoroutines"],
})

_BUILD_MODE_CXX_FLAGS = select({
    "root//buck2/constraints:opt": select({
        "root//buck2/constraints:asan": ["-g"],
        "DEFAULT": ["-O3", "-DNDEBUG"],
    }),
    "DEFAULT": ["-g"],
})

_COMMON_CXX_FLAGS = (
    ["-Wno-nullability-completeness", "-fno-omit-frame-pointer"] +
    _COROUTINES_FLAGS + _ASAN_CXX_FLAGS + _BUILD_MODE_CXX_FLAGS
)

_HASWELL_FLAGS = select({
    "prelude//cpu:x86_64": ["-march=haswell"],
    "DEFAULT": [],
})

def cxx_library(name, compiler_flags = [], linker_flags = [], cxx_std = True, **kwargs):
    std_flags = ["-std=c++20"] if cxx_std else []
    native.cxx_library(
        name = name,
        compiler_flags = std_flags + compiler_flags + _COMMON_CXX_FLAGS + _HASWELL_FLAGS,
        # -fsanitize=address has to reach the *linker* too, not just the
        # compiler - it isn't just an instrumentation flag, it also tells
        # gcc/clang to link the ASan runtime in; a .so whose objects were
        # compiled with it but linked without it is missing that runtime
        # (confirmed directly: exactly this gap produced "undefined
        # symbol: __asan_option_detect_stack_use_after_return" the first
        # time asan mode touched anything with a shared-library dependency,
        # here hsthrift's own folly-clib).
        linker_flags = _ASAN_CXX_FLAGS + linker_flags,
        **kwargs
    )

# Same centralized flags for cxx_binary() - so far only glean-clang's
# `clang-index` (a genuine plain C++ executable, not Haskell-with-C++-
# sources) needs this, but the flags themselves are identical to
# cxx_library()'s, so it's a duplicate of this wrapper rather than a
# hand-rolled select() at the one call site.
def cxx_binary(name, compiler_flags = [], linker_flags = [], cxx_std = True, **kwargs):
    std_flags = ["-std=c++20"] if cxx_std else []
    native.cxx_binary(
        name = name,
        compiler_flags = std_flags + compiler_flags + _COMMON_CXX_FLAGS + _HASWELL_FLAGS,
        linker_flags = _ASAN_CXX_FLAGS + linker_flags,
        **kwargs
    )
