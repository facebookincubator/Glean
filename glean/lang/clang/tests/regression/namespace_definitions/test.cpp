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
}
