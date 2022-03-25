/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

struct Foo {};
struct Bar {};
struct Baz {};

template<typename T>
struct S {
  Foo x;
  T y;
};

template<typename T>
struct S<T*> {
  Foo x;
  T* y;
};

S<Bar> x1;
S<Baz> x2;
S<Bar*> x3;
