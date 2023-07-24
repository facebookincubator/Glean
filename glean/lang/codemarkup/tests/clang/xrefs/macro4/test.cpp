/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "a.h"

namespace N {
  void foo() {}
}

void bar() {
  REF_NONMACRO;
}

void baz() {
  { REF_MACRO }
  { REF_MACRO }
  { REF_MACRO }
}
