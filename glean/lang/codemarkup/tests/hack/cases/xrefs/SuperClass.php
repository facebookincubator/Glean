<?hh
/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

class SuperClass {

  const int BAZ = 999;

  public function superAdd(int $i, int $i): int {
    return $i + $i + 55;
  }

  public function overrideMe(int $i): int {
    return $i;
  }
}
