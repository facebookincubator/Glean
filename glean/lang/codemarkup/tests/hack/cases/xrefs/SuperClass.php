<?hh // strict
/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

class SuperClass {

  const int BAZ = 999;

  public function superAdd(int $i, int $j): int {
    return $i + $j + 55;
  }
}
