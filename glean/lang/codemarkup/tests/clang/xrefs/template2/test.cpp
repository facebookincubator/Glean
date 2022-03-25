/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

template<typename T> struct A {
  template<typename U> static void f(U);
};

template<>
template<>
void A<int*>::f<int>(int) {}

void g() {
  A<int>::f(0);
}
