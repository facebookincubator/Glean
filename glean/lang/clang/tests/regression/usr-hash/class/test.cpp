/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

namespace bar {
  struct bar_st {
    int f1;
    int f2;
  };
}

namespace foo {
  class c1 {
    public:
      virtual int f0() {
        return 1;
      }

    private:
      int f1() {
        return 0;
      }
  };
  class c2 : public c1 {
    public:
      int f0() override {
        return 2;
      }
  };
}
