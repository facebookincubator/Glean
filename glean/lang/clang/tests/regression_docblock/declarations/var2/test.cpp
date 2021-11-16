/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

/// @testname declarations/var2
/// @test struct T
struct T {
  /// @testname declarations/var2
  /// @test int m1
  int m1;
  /// @testname declarations/var2
  /// @test int m2 : 5
  int m2 : 5;
  /// @testname declarations/var2
  /// @test mutable int m3
  mutable int m3;
};
