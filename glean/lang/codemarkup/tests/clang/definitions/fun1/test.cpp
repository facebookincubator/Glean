/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

void f1() {
}
void f2(bool x, int y) {
}

namespace bar {
  struct bar_st {
    int f1;
    int f2;
  };
}

namespace foo {
  void f3(int, char, char) {
  }
  int f4(bar::bar_st &b) {
    return 0;
  }
  static void f5() {
  }

  class c1 {
    public:
      c1() {
      }

    private:
      int f1() {
        return 0;
      }
  };
}
