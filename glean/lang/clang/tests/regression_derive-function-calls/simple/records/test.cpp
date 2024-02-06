/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

struct A {
  int x;
};

struct B : A {                  // should reference A
  int y;
};

struct C {
  B b;
};

struct D {
  void f () {
    A a;                        // D should reference A
  }
  void g();
};

void D::g() {                   // D does not reference B
  B b;
}

class E {
  private:
  C c;                          // E should reference C
};
