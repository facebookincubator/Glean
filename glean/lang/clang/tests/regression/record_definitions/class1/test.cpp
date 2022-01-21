/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

struct T {};
struct U {
  U();
  U(const U&);
  U& operator=(const U&);
};

struct A {
  virtual ~A() = 0;
};

struct B : A {
  U u;
  int lucky() { return 0; };

  int varB;
};
