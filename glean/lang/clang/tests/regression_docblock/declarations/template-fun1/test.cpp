/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

/**
 * @testname declarations/template-fun1
 * @type f T
 */
template<typename T> T f(T x) {
  return x;
}

/**
 * @testname declarations/template-fun1
 * @type f T*
 */
template<typename T> T* f(T* x) {
  return x;
}

/**
 * @testname declarations/template-fun1
 * @type g
 */
void g(int x) {
  f(x);
  f(&x);
  f("");
}
