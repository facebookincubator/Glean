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

struct C {
  struct S {};

  template <typename>
  struct ST {};

  void f() {}

  template <typename>
  void ft() {}

  enum E { EA, EB, EC };
  enum class EC { ECA, ECB, ECC };

  static constexpr int v = 42;

  template <typename T>
  static constexpr T vt = 42;

  int m;

  using U = T;

  template <typename X>
  using UT = X;

  typedef S TD;
};
