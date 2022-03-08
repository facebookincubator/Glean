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
  support for other OSs in the future. We have tested on x86\_64 and arm64v8.

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
    ninja-build \
    bison flex \
    git \
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
    make \
    zlib1g-dev \
    binutils-dev \
    libjemalloc-dev \
    libssl-dev \
    pkg-config \
    libunwind-dev \
    libsodium-dev \
    curl \
    libpcre3-dev \
    libmysqlclient-dev \
    libfftw3-dev \
    libxxhash-dev
```

### Debian

The package dependencies for Debian current are the same as above for Ubuntu,
except you need `default-libmysqlclient-dev` instead of `libmysqlclient-dev`.

### Fedora

Install prerequisite packages:

```
sudo dnf install \
    g++ \
    make \
    cmake \
    ninja-build \
    binutils \
    bison flex \
    curl \
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
    community-mysql-devel \
    fftw-devel \
    xxhash-devel \
    snappy-devel \
    lz4-devel
```

## Building

Clone the repository:

```
git clone https://github.com/facebookincubator/Glean.git
cd Glean
```

### Build hsthrift and dependencies

Glean depends on hsthrift, fbthrift, folly, rocksdb and some other core libraries.
We need to set paths to these that the Glean build can find the thrift compiler
and associated libraries:

```
export LD_LIBRARY_PATH=$HOME/.hsthrift/lib:$HOME/.hsthrift/lib64:$LD_LIBRARY_PATH
export PKG_CONFIG_PATH=$HOME/.hsthrift/lib/pkgconfig:$HOME/.hsthrift/lib64/pkgconfig
export PATH=$PATH:$HOME/.hsthrift/bin
```

Now clone [hsthrift](https://github.com/facebookincubator/hsthrift) and
build and install its dependencies:
```
./install_deps.sh
```

You can speed up builds by passing e.g. `--threads 8` if you have available hardware.

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

