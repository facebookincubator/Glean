---
id: cxx
title: C++ and C
sidebar_label: C++ and C
---

import {OssOnly, FbInternalOnly} from 'internaldocs-fb-helpers';
import {SrcFile,SrcFileLink} from '@site/utils';

The C++ indexer ("the clang indexer") is a wrapper over
[clang](https://clang.llvm.org/). The clang indexer is a drop in replacement
for the a C++ compiler that emits Glean facts instead of code. The wrapper is
linked against libclang and libllvm.

Glean supports simple cmake builds out of the box. For other build systems
you'll need to run clang-index manually on compilation targets. The indexer has
been designed to scale up to very large repos, indexing compilation units in
parallel across multiple machines.

To build the indexer, you will need clang-11, libclang-11, llvm-11. See the
[github CI
workflow](https://github.com/facebookincubator/Glean/blob/main/.github/workflows/ci.yml)
for the specific dependencies we test with.

The indexer consists of two parts:

- clang-index

which compiles the C++ source and emits binary thrift data to write to Glean.

- clang-derive

which computes derived facts on the result (e.g. find-references tables).

## To build the indexer:

> make glean-clang

## Run the indexer

The cmake-based indexer can run via the main `glean` CLI tool.

```
> cabal build exe:glean
```

And index your c++ repository with:
```
glean index cpp DIR --indexer clang-index --deriver clang-derive --repo name/hash
```

where

* `DIR` is the root directory containing the CMake manifest.
* and indexer and deriver are the paths to the clang-index and clang-derive binaries

## In the shell

C++ source in cmake projects can also be indexed directly from the Glean shell:

```
:index cpp DIR
```

Note that clang-index and clang-derive should be built and accessible in the
PATH variable for this to succeed, or in the build tree.

## Schema

The schema is in <SrcFile file="glean/schema/source/cxx1.angle" />

The schema is quite rich and captures C++, C, Objective-C and C pre-processor
symbols, the semantic structure of C++ symbols, and is precise enough to do
automated analysis of C++ code.
