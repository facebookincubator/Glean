/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

namespace N1 {
enum Foo {X};
}

namespace N2 {
template<typename T> struct Bar {};
template<typename T> void bar() {}
}

namespace N3 {
using N1::Foo;
N2::Bar<Foo> x;
void f() {
  N2::bar<Foo>();
}
}
