/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

/// @testname declarations/var1
/// @test extern int v1
extern int v1;
/// @testname declarations/var1
/// @test int v1
int v1;
/// @testname declarations/var1
/// @test static int v2
static int v2;
/// @testname declarations/var1
/// @test inline int v3
inline int v3;
/// @testname declarations/var1
/// @test constexpr int v4
constexpr int v4 = 4;

struct T {
  /// @testname declarations/var1
  /// @test static int m1
  static int m1;
  /// @testname declarations/var1
  /// @test int m2
  static int m2;
  /// @testname declarations/var1
  /// @test inline static int m3
  inline static int m3;
  /// @testname declarations/var1
  /// @test constexpr static int m4
  constexpr static int m4 = 4;
};

/// @testname declarations/var1
/// @test int T::m1
int T::m1;
/// @testname declarations/var1
/// @test int T::m2
inline int T::m2;

/// @testname declarations/var1
/// @test void f
void f(int x1) {
  static int l1; // shouldn't be indexed
  int l2; // shouldn't be indexed
  [t=l2] {};
}
