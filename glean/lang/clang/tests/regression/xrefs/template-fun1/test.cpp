/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

struct Foo { void mem(); };
struct Bar { void mem(); };
struct Baz { void mem(); };

void global();
void apply(Foo);
void apply(Bar);
void apply(Baz);

template<typename T> void foo(T x) {
  global();
  apply(x);
  x.mem();
}

template<> void foo<>(Foo* x) {
  x->mem();
  apply(*x);
  global();
}

template void foo<Baz>(Baz);

void h() {
  Foo x;
  Bar y;
  Baz z;
  foo(x);
  foo(y);
  foo(&x);
  foo(z);
}
