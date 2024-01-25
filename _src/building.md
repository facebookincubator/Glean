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

* [GHC](https://www.haskell.org/ghc/). To see which versions Glean is tested with, check the current [ci.yml](https://github.com/facebookincubator/Glean/blob/master/.github/workflows/ci.yml) script.

* Cabal/cabal-install version 3.6 or later (older versions won't work).

Additional per-distribution setup follows.

### Ubuntu

Install prerequisite packages. (many of these are dependencies of
hsthrift; an up to date list can be found in the
[Dockerfile](https://github.com/facebookincubator/hsthrift/blob/master/.github/workflows/Dockerfile)
that we use for building the base image for CI).

```
sudo apt-get install \
    g++ \
    cmake \
    make \
    ninja-build \
    bison flex \
    git curl \
    rsync m4 \
    libzstd-dev \
    libboost-all-dev \
    libevent-dev \
    libdouble-conversion-dev \
    libgoogle-glog-dev \
    libgflags-dev \
    libiberty-dev \
    liblz4-dev \
    liblzma-dev \
    libsnappy-dev \
    zlib1g-dev \
    binutils-dev \
    libjemalloc-dev \
    libssl-dev \
    pkg-config \
    libunwind-dev \
    libsodium-dev \
    libpcre3-dev \
    libfftw3-dev \
    libxxhash-dev \
    libgtest-dev \
    libfmt-dev \
    clang-12 \
    llvm-12 \
    libclang-12-dev
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
    llvm12-devel
```

## Building

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
