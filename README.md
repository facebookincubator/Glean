[![CI](https://github.com/facebookincubator/Glean/actions/workflows/ci.yml/badge.svg)](https://github.com/facebookincubator/Glean/actions/workflows/ci.yml)
[![CI-ARM64](https://github.com/facebookincubator/Glean/actions/workflows/ci-arm64.yml/badge.svg)](https://github.com/facebookincubator/Glean/actions/workflows/ci-arm64.yml)
[![CI-Clang](https://github.com/facebookincubator/Glean/actions/workflows/ci-clang-rocksdb.yml/badge.svg)](https://github.com/facebookincubator/Glean/actions/workflows/ci-clang-rocksdb.yml)
[![Glean demo Docker image](https://github.com/facebookincubator/Glean/actions/workflows/glean-docker.yml/badge.svg)](https://github.com/facebookincubator/Glean/actions/workflows/glean-docker.yml)

# Glean

[Glean home](https://glean.software) | [Documentation](https://glean.software/docs/introduction)

Glean is a system for working with facts about source code. You can
use it for:

* Collecting and storing detailed information about code
  structure. Glean is designed around an efficient storage model that
  enables storing information about code at scale.

* Querying information about code, to power tools and experiences from
  online IDE features to offline code analysis.

## Status

Glean is pre-release software. There are many rough edges; there are
limited language indexers available initially; the build system is not as smooth
as we would like. However, we want to make it available for you to experiment
with and contribute to.

We'd love to hear feedback! If you run into problems or have
suggestions please file an
[issue](https://github.com/facebookincubator/Glean/issues).

## Language coverage

There is currently full support for:

* [C++ and C](https://glean.software/docs/indexer/cxx)
* [Hack](https://glean.software/docs/indexer/hack)
* [Haskell](https://glean.software/docs/indexer/haskell)
* [JavaScript/Flow](https://glean.software/docs/indexer/flow)

We also support the [SCIP](https://github.com/sourcegraph/scip) or [LSIF](https://lsif.dev) code indexing formats, for:

* [Rust (via rust-analyzer)](https://glean.software/docs/indexer/lsif-rust)
* [Go](https://glean.software/docs/indexer/lsif-go)
* [TypeScript](https://glean.software/docs/indexer/lsif-typescript)
* [Java](https://glean.software/docs/indexer/lsif-java)

Indexers for these languages exist but aren't in the open source
release yet; we hope to make these available in the future:

* Python
* Objective-C
* Java
* Kotlin
* Erlang
* Thrift
* Buck

## License

Glean is licensed under a [BSD LICENSE](LICENSE).

## Contributing

We'd love to have your help developing Glean. Please submit [pull
requests](https://github.com/facebookincubator/Glean/pulls) on github,
and note that we need contributors to sign the [Contributor license
agreement](https://code.facebook.com/cla) before we can accept your
pull request.

Style guide: for Haskell code, we use `hlint` to check for style issues. Lines
wrap at 80 columns. Code should be -Wall clean. For C++, code should compile
with Clang or GCC.

## How to contact the Glean team

- Visit [the project GitHub repo](https://github.com/facebookincubator/Glean) to view the source code, open issues or pull requests.
- Join the [Glean Discord server](https://discord.com/channels/280033776820813825/505370075402862594/808027763868827659). You can join the server via [this invitation](https://discord.gg/w3s6X6QAHZ).
## Building

See [Building Glean](https://glean.software/docs/building).

## Docker demo

For demo of the react codebase with hyperlinks powered by glean run
`docker run -ti -p8888:8888 ghcr.io/facebookincubator/glean/demo`

Try out on your own codebase with a .flowconfig by running
`docker run -ti -p8888:8888 -v __YOUR_CODE_DIR__:/glean_demo/code ghcr.io/facebookincubator/glean/demo`

Play round using the glean binaries in a shell by running
`docker run -ti -p8888:8888 ghcr.io/facebookincubator/glean/demo shell`
