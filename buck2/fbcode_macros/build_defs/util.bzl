# Shared by thrift_library.bzl/haskell_library.bzl: translates target
# references from gen-schema's checked-in-but-unmodified, @fbcode_macros-
# shaped glean/schema/{thrift,hs}/BUCK files to the real buck2 targets this
# migration actually builds. See buck2.md's "gen-schema" entry for how each
# of these was established (mostly: languages/backends this migration
# doesn't support - cpp2/py/py3/rust/java-swift - collapse away entirely,
# and Meta-internal per-file annotation/hs2 targets collapse onto the one
# real target this repo built for each).
_DEP_ALIASES = {
    "//glean/if:glean": "//glean/if:if-glean-hs",
    "//glean/if:glean-hs2": "//glean/if:if-glean-hs",
    "//thrift/annotation:cpp": "//thrift/annotation:thrift-annotation",
    "//thrift/annotation:haskell": "//thrift/annotation:thrift-annotation",
    "//thrift/annotation:rust": "//thrift/annotation:thrift-annotation",
    "//thrift/annotation:thrift": "//thrift/annotation:thrift-annotation",
    "//glean/typed:query-angle": "//glean/typed:typed",
}

def translate_deps(deps):
    out = []
    for d in deps:
        d = _DEP_ALIASES.get(d, d)
        if d not in out:
            out.append(d)
    return out
