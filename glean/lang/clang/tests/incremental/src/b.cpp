/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "a.h"

namespace test {

int b() {
  return 4; // changed result
}

/** New doc comment for bb
 * @type int bb
 */
int bb(int x, int y) { // added parameter
  C c;
  return c.f() + x + y;
}

}
