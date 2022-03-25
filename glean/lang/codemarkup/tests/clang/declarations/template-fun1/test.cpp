/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

template<typename T> T f(T x) {
  return x;
}

template<typename T> T* f(T* x) {
  return x;
}

void g(int x) {
  f(x);
  f(&x);
  f("");
}
