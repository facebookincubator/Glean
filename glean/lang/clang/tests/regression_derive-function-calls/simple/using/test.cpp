/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

namespace N {
  void g() {}
}

using N::g;

void f() {
  // test that we correctly capture references that go through a using
  // declaration.
  g();
}
