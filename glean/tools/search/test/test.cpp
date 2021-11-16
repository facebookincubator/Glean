/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "test.h"

namespace facebook {

int global = 3;

void f(int x) {}

C::C() { a = 0; }
C::C(int x) : a(x) { }

C::~C() {}

int C::get() { return a; }

}

using namespace facebook;

int main() {
  C c { global };

  return TEST(c.get());
}
