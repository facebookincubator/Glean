/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

struct T {
  int m1;
  int m2 : 5;
  mutable int m3;

  void f() {
    m1;
    m2;
    m3;
  }
};

void g(T x, T *y) {
  x.m1;
  x.m2;
  x.m3;
  y->m1;
  y->m2;
  y->m3;
  &T::m1;
  &T::m3;
}
