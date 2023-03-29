/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

namespace N1 {
  namespace N2 {}

  struct S {};

  template <typename T>
  struct ST {};

  void f() {}

  template <typename T>
  void ft(T) {}

  enum E { A, B, C };
  enum class EC { CA, CB, CC };

  inline constexpr int v = 42;

  template <typename T>
  inline constexpr T vt = 42;

  using U = S;

  template <typename T>
  using UT = T;

  typedef S TD;

  template <typename T>
  struct ST2 {
    static bool value;
  };

  template <typename T>
  bool ST2<T>::value;
}

void f() {
  N1::ST<int> s1;
  N1::ST<double> s2;
  N1::ft(1);
  N1::ft(1.0);
  (void)N1::vt<int>;
  (void)N1::vt<double>;
  N1::UT<int> u1;
  N1::UT<double> u2;
  (void)N1::ST2<int>::value;
  (void)N1::ST2<double>::value;
}
