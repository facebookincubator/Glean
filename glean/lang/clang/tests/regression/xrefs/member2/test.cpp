/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

struct S {
  S() = default;
  S(const S&) = default;

  int x;
};

void f() {
  S s1;
  S s2(s1);
}
