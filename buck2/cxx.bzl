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
        # -fsanitize=address has to reach the *linker* too
        linker_flags = _ASAN_CXX_FLAGS + linker_flags,
        **kwargs
    )

def cxx_binary(name, compiler_flags = [], linker_flags = [], cxx_std = True, **kwargs):
    std_flags = ["-std=c++20"] if cxx_std else []
    native.cxx_binary(
        name = name,
        compiler_flags = std_flags + compiler_flags + _COMMON_CXX_FLAGS + _HASWELL_FLAGS,
        linker_flags = _ASAN_CXX_FLAGS + linker_flags,
        **kwargs
    )
