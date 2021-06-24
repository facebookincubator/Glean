---
id: building
title: Building Glean from Source
sidebar_label: Building Glean
---

import {OssOnly, FbInternalOnly} from 'internaldocs-fb-helpers';

## You will need

* Linux. The build is only tested on Linux so far; we hope to add
  other OSs in the future.

* [GHC](https://www.haskell.org/ghc/). To see which versions Glean is tested with, check the current [ci.yml](https://github.com/facebookincubator/Glean/blob/master/.github/workflows/ci.yml) script.

* Cabal/cabal-install. Unfortunately because hsthrift currently depends on features that are not in a released version of Cabal (3.6+), you have to build it from source, see https://github.com/facebookincubator/hsthrift#building-and-testing

* Lots of packages:
  * Ubuntu:
    * The hsthrift dependencies can be found in the [Dockerfile](https://github.com/facebookincubator/hsthrift/blob/master/.github/workflows/Dockerfile) for the base image
    * Additionally for Glean, you will need `librocksdb-dev` and `libxxhash-dev`
  * Fedora:
    * (TODO)

## Building

:::warning

The build process currently installs dependencies in
`/usr/local/lib`. This isn't ideal; we're working on a more
self-contained build process but it's not ready yet.

:::

Clone the repository:

```
git clone https://github.com/facebookincubator/Glean.git
cd glean
```

These are necessary so that the Glean build can find the dependencies
that get installed in `/usr/local/lib`:

```
export LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH
export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig
```

Clone [hsthrift](https://github.com/facebookincubator/hsthrift) and
install its dependencies:

```
./install_deps.sh
```

Build everything:

```
make thrift
make gen-bytecode
make gen-schema
make thrift-schema-hs
make glean
```

If everything worked, the tests should pass:

```
make test
```

At this point you can `cabal install` to install the executables into
`~/.cabal/bin`.
