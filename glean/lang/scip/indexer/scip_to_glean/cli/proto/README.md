# SCIP Protocol Buffer Definition

`scip.proto` is from the [SCIP project](https://github.com/sourcegraph/scip).

To update it, copy the latest version from upstream:

```
curl -o scip.proto https://raw.githubusercontent.com/sourcegraph/scip/main/scip.proto
```

Rust bindings are generated automatically by `build.rs` using
`protobuf-codegen` during `cargo build`.
