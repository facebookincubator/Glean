# Clang ACL snapshot tests

`acl.yaml` defines path ACLs using the canonical identities returned by SCS,
and optional database properties. Each leaf test directory has a
`scenario.yaml` defining the caller's identities and, when needed, the pruned
incremental database configuration.

Query files are inherited by descendant test directories. Put queries shared
by every scenario here, base-only queries under `base/`, and incremental-only
queries under `incremental/`.

Regenerate the expected `.out` files with:

```sh
buck2 test -c glean.test=replace fbcode//glean/lang/clang/tests:acl
```
