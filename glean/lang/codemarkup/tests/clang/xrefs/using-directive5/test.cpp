/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

namespace N1 {

struct T {};

}

using N1::T;

namespace N2 {
namespace N21 {

using namespace N2;

void foo() {
  T x;
}

}
}
