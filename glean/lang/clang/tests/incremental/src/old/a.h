/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

namespace test {

/** Docs for class C
 * @class C
 * @testname incremental
 */
class C {
  public:
  int f();

  private:
  int x;
};

int a(); // defined in a.cpp

int b(); // defined in b.cpp

}
