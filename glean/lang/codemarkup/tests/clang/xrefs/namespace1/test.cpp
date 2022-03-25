/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

namespace foo {

struct S {};
void f();

namespace bar {

struct T {};
void g();

}
}

void h() {
  foo::S s;
  foo::bar::T t;
  foo::f();
  foo::bar::g();
}
