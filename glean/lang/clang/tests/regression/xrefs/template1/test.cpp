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

auto f1 = x1.x;
auto t1 = x1.y;
auto f2 = x2.x;
auto t2 = x2.y;
auto f3 = x3.x;
auto t3 = x3.y;
