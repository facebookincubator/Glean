/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

namespace foo {

struct S {};

/**
 * Test Code Block
 */
void f();

/* test/cpp/foo/foo */
void foo() {
  return;
}

/* check duplicate symbols in same namespace */
/* test/cpp/foo/foo */
void foo();

namespace bar {

struct T {};
void g();

}
}

void h() {
  foo::S s;
  foo::bar::T t;
  foo::f();
  foo::foo();
  foo::bar::g();
}

#include "test.h"

#define A "one"
#define B "two"

