/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#define PASTE(x,y) x##y
#define NESTED_PASTE(x,y) PASTE(x,y)

#define MACRO_1(x) x
#define MACRO_2(x) MACRO_1(x)
#define MACRO_3(x) MACRO_2(x)

void foobar() {}

void f() {
  MACRO_1(PASTE(foo,bar))();
  MACRO_2(PASTE(foo,bar))();
  MACRO_3(PASTE(foo,bar))();

  NESTED_PASTE(foo,bar)();
}
