![ci](https://github.com/facebookincubator/Glean/actions/workflows/ci.yml/badge.svg)
![demo](https://github.com/facebookincubator/Glean/actions/workflows/glean-docker.yml/badge.svg)

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
only two language indexers available initially (JavaScript and Hack); the
build system is not as smooth as we would like. However, we want to
make it available for you to experiment with and contribute to.

We'd love to hear feedback! If you run into problems or have
suggestions please file an
[issue](https://github.com/facebookincubator/Glean/issues).

## Language coverage

There is currently support for:

* [JavaScript/Flow](https://glean.software/docs/indexer/flow)
* [Hack](https://glean.software/docs/indexer/hack)

Indexers for these languages exist but aren't in the open source
release yet; we hope to make these available in the future:

* Python
* C++
* Objective-C
* Java
* Rust

## License

Glean is licensed under a [BSD LICENSE](LICENSE).

## Contributing

We'd love to have your help developing Glean. Please submit [pull
requests](https://github.com/facebookincubator/Glean/pulls) on github,
and note that we need contributors to sign the [Contributor license
agreement](https://code.facebook.com/cla) before we can accept your
pull request.

## Building

See [Building Glean](https://glean.software/docs/building).
