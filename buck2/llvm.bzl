# Discovers this system's LLVM/Clang installation via `llvm-config-
# <version>`, the same way `buck2/prelude/third-party/pkgconfig.bzl`'s
# external_pkgconfig_library() discovers a pkg-config-described system
# library: a couple of genrule()s shelling out to the real tool at build
# time (its output describes *this machine's* install - absolute paths
# under e.g. /usr/lib/llvm-15 - not something safe to bake into checked-
# in BUCK text or share across machines via remote execution, same
# reasoning pkgconfig.bzl's own comment gives for `remote = False`
# there), wrapped in a prebuilt_cxx_library() so a consumer just adds it
# to its own `deps` like any other library, instead of hand-rolling
# `-I`/`-L`/`-l` flags itself.
#
# llvm-config's own `--cxxflags` bundles its include path and defines
# together with `-std=c++14 -fno-exceptions` - dropped here by keeping
# only the `-I`/`-D` tokens from that output, not passed through whole
# (see glean/lang/clang/BUCK's own comment on why: that package's
# sources need >= C++17 and don't build with exceptions disabled, so
# whatever it links against can't be pinned to llvm-config's own
# defaults for those two).
def external_llvm_config_library(name, version, visibility = ["PUBLIC"]):
    llvm_config = "llvm-config-{}".format(version)

    cflags = name + "__llvm_config_cflags"
    native.genrule(
        name = cflags,
        out = "out",
        cmd = "{ll} --cxxflags | tr ' ' '\\n' | grep -E '^-[ID]' | tr '\\n' ' ' > $OUT".format(ll = llvm_config),
        remote = False,
    )

    libs = name + "__llvm_config_libs"
    native.genrule(
        name = libs,
        out = "out",
        cmd = "{ll} --ldflags > $OUT; {ll} --libs >> $OUT; {ll} --system-libs >> $OUT".format(ll = llvm_config),
        remote = False,
    )

    native.prebuilt_cxx_library(
        name = name,
        visibility = visibility,
        exported_preprocessor_flags = ["@$(location :{})".format(cflags)],
        exported_linker_flags = ["@$(location :{})".format(libs)],
    )
