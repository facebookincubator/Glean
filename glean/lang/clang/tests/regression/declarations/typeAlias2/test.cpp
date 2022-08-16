/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

template <typename T>
struct Foo {
  using T1 = T;
  struct Bar {
    using T2 = T;
  };
};

template <typename T>
struct Foo<Foo<T>> {
  using T3 = T;
  struct Bar {
    using T4 = T;
  };
};

Foo<int>::T1 t1;
Foo<short>::Bar::T2 t2;
Foo<Foo<short>>::T3 t3;
Foo<Foo<short>>::Bar::T4 t4;
