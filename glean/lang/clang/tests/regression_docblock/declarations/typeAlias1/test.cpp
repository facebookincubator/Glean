/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

/// @testname declarations/typeAlias1
/// @test typedef int T;
typedef int T;

/// @testname declarations/typeAlias1
/// @test using U = T;
using U = T;

T x;
U y;
