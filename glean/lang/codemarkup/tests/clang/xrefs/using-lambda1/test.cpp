/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

namespace N1 {

inline void foo() {}

}

void g() {
  auto bar = [&]() {
    using namespace N1;
    foo();
  };
  bar();
}

void h() {
  auto bar = [&](auto) {
    using namespace N1;
    foo();
  };
  bar(1);
  bar("");
}
