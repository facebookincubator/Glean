# Shim for @fbcode_macros//build_defs:custom_rule.bzl. glean/schema/thrift/
# BUCK loads this but (as of this migration) never actually calls it - see
# buck2.md's "gen-schema" entry. Defined so the load() succeeds; fails
# loudly if that ever changes, rather than silently doing nothing.
def custom_rule(name, **_kwargs):
    fail("custom_rule({}): not implemented by this migration's fbcode_macros shim".format(name))
