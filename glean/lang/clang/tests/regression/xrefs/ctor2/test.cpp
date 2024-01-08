/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

struct X {};

struct S1 {
  S1() : x1() {}

  X x1;
  X x2;
};

struct S2 {
  S2();

  X x1;
  X x2;
};

S2::S2() : x2() {}
