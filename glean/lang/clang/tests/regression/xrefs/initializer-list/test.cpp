/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

struct T {
  T(int x) {}
};

struct S : T {
  S(int y) :
      x_(y), T(3) {}
  int x_;
};
