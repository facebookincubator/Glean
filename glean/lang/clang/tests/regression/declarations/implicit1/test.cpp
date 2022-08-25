/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

struct A {};

void f() {
  A a;
  A a1(a);
  A a2((A&&)a);
  a = a1;
  a = (A&&)a2;
}

template <typename T>
struct B {};

void g() {
  {
    B<int> b;
    B<int> b1(b);
    B<int> b2((B<int>&&)b);
    b = b1;
    b = (B<int>&&)b2;
  }
  {
    B<short> b;
    B<short> b1(b);
    B<short> b2((B<short>&&)b);
    b = b1;
    b = (B<short>&&)b2;
  }
}
