/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

namespace N1 {

enum X { X1, X2, X3 };
enum class Y { Y1, Y2, Y3 };

}

void foo() {
  using namespace N1;

  X1;
  Y::Y1;
}

void bar () {
  using N1::X1;
  using N1::Y;

  X1;
  Y::Y1;
}
