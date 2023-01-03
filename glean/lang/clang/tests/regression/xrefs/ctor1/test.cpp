/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

struct B {
  B() = default;
};

struct X {
  X(int) {}
};

struct S : B {
  S(int i) : B(), x(i) {}

  X x;
};

void f() {
  int i = 42;
  S s1(i);
  S s2 = i;
  S s3{i};
  S s4 = {i};
}

S g() {
  int i = 42;
  return i;
}
