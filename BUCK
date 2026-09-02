load("//buck2:cxx.bzl", "cxx_library")

# Header-only. glean/schema/cpp/schema.h is a gitignored, locally
# generated copy of gen-schema's C++ output (glean/lang/clang/BUCK's own
# comment explains the twin copy at glean/lang/clang/schema.h, and the
# Makefile rule that keeps them in sync) - not yet a genrule() output
# under buck2, just checked into the filesystem like the rest of the
# still-Makefile-generated schema artifacts this migration hasn't ported
# yet. glean/schema/cpp has no BUCK file of its own, so it - and this
# file - belong to the repo root package, which is why this target lives
# here rather than somewhere more specific.
cxx_library(
    name = "schema-cpp-header",
    exported_headers = [
        "glean/schema/cpp/schema.h",
    ],
    visibility = ["PUBLIC"],
)
