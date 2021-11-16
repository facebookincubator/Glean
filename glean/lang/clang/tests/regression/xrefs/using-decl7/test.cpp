/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

namespace N1 {
template<typename T> void foo() {}
}

using N1::foo;

template<typename T> void bar() {
  foo<T>();
}

namespace N2 {
void foo(int);
}

namespace N3 {
void foo(bool);
}

namespace N4 {
using N2::foo;
using N3::foo;

template<typename T> void bar(T x) {
  foo(x);
}

}
