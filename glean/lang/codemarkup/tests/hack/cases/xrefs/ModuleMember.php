<?hh
/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

module glean.test.thing;

function fun1<T>(T $id): T {
  return $id;
}

internal function fun2<T>(T $id, T $other): T {
  return $other;
}

internal type IntT = int;
