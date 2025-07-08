---
id: building
title: Building Glean from Source
sidebar_label: Building Glean
---

import {OssOnly, FbInternalOnly} from 'internaldocs-fb-helpers';

## Introduction

Glean is written mainly in [Haskell](http://www.haskell.org/), and
makes heavy use of
[Thrift](https://github.com/facebookincubator/hsthrift) for data
communication and serialization, so these are the main dependencies
that are needed for building Glean.

## You will need

* Linux. The build is only tested on Linux so far; we hope to add
  support for other OSs in the future. We build on x86\_64 and arm64v8.

* [GHC](https://www.haskell.org/ghc/). To see which versions Glean is tested with, check the current [ci.yml](https://github.com/facebookincubator/Glean/blob/master/.github/workflows/ci.yml) script. We recommend installing GHC using [ghcup](https://www.haskell.org/ghcup/).

* Cabal/cabal-install version 3.6 or later (older versions won't
  work). As for GHC, install these using ghcup.

Additional per-distribution setup follows.

### Ubuntu

Our CI runs on Ubuntu, so this is the most well-tested platform. If
you encounter problems, the source of truth for what works is the
[github CI workflow](https://github.com/facebookincubator/Glean/blob/main/.github/workflows/ci.yml). In
there you can also find the version of Ubuntu we test on; older
versions may or may not work, in particular using a different version
of `librocksdb-dev` is likely to run into problems.

Install these prerequisite packages:

```
sudo apt-get install \
    g++ \
    cmake \
    make \
    ninja-build \
    bison \
    flex \
    git \
    curl \
    rsync \
    m4 \
    pkg-config \
    binutils-dev \
    libboost-all-dev \
    libdouble-conversion-dev \
    libdwarf-dev \
    libevent-dev \
    libfast-float-dev \
    libfftw3-dev \
    libfmt-dev \
    libgflags-dev \
    libgmock-dev \
    libgoogle-glog-dev \
    libgtest-dev \
    libiberty-dev \
    libjemalloc-dev \
    liblz4-dev \
    liblzma-dev \
    libpcre3-dev \
    librocksdb-dev \
    libsnappy-dev \
    libsodium-dev \
    libssl-dev \
    libtinfo-dev \
    libunwind-dev \
    libxxhash-dev \
    libzstd-dev \
    zlib1g-dev
```

Additionally if you want to build the C++ indexer:

```
apt install clang-15 libclang-15-dev libclang-cpp15-dev libre2-dev
```

### Debian

The package dependencies for Debian current are the same as above for Ubuntu.

### Fedora

Install prerequisite packages:

```
sudo dnf install \
    g++ \
    make \
    cmake \
    curl git \
    rsync m4 \
    ninja-build \
    binutils \
    bison flex \
    libzstd-devel \
    boost-devel \
    libevent-devel \
    double-conversion-devel \
    glog-devel \
    gflags-devel \
    zlib-devel \
    openssl-devel \
    libunwind-devel \
    libsodium-devel \
    pcre-devel \
    fftw-devel \
    xxhash-devel \
    snappy-devel \
    lz4-devel \
    gtest-devel \
    fmt-devel \
    xxhash \
    clang \
    llvm12-devel \
    libfast-float-dev
```

## Build using Cabal

Glean can be built and installed entirely using `cabal`, with:

```
cabal install glean
```

This will install the following executables in `~/.cabal/bin`:
* `glean`: the [command-line tool](cli.md) for doing most things
* `glean-server`: to run a [server](server.md)
* `glass-server`: the [Glass](https://github.com/facebookincubator/Glean/tree/main/glean/glass) symbol server

The schema source files will be installed somewhere under
`~/.cabal/store` (Glean knows where to find them).

Installing with `cabal install` is sufficient if you want to:

* Just try it out
* Index some source code in a supported language
* Run queries against a DB, perhaps produced by someone else
* Run a server (including Glass)
* Build something that depends on Glean

If you want to make changes to the schema or work on Glean itself then
you will likely need to build from source: continue to the next
section.

## Building from the repository

Clone the repository:

```
git clone https://github.com/facebookincubator/Glean.git
cd Glean
```

### Build hsthrift and dependencies

Glean depends on hsthrift, folly, rocksdb and some other core libraries.
We need to set paths to these that the Glean build can find the thrift compiler
and associated libraries:

```
export LD_LIBRARY_PATH=$HOME/.hsthrift/lib:$HOME/.hsthrift/lib64:$LD_LIBRARY_PATH
export PKG_CONFIG_PATH=$HOME/.hsthrift/lib/pkgconfig:$HOME/.hsthrift/lib64/pkgconfig
export PATH=$PATH:$HOME/.hsthrift/bin
```

On Fedora/CentOS and possibly other non-Debian systems, you also need to set:
```
export LIBRARY_PATH=$HOME/.hsthrift/lib:$HOME/.hsthrift/lib64:$LIBRARY_PATH
```

These will build with either gcc or clang as the base C and C++ compilers. Clang is
the best supported C++ compiler for this project.

Now we will clone [hsthrift](https://github.com/facebookincubator/hsthrift) and
build and install its dependencies:
```
./install_deps.sh
```

### Build Glean

Now you can build all the Glean parts:

```
cabal update
make
```

If everything worked, the tests should pass:

```
make test
```

At this point you can `cabal install` to install the executables into
`~/.cabal/bin`.

### Tips for faster builds

If you have 4 or more cores and at least 16G of ram, you can significantly speed up the build times by passing some flags to the build stages.
On an 6 core machine with 16G of ram you might use, to save 50% or more of the build time.

```
./install_deps.sh --threads 6
make EXTRA_GHC_OPTS='-j4 +RTS -A128m -n2m -RTS'
```

Using clang++-12 and clang-12 as the C and C++ compilers can shave another 25% off the build time.
