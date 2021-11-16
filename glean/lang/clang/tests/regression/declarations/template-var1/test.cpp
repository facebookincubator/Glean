/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

template<typename T>
struct U1 {
  unsigned x : sizeof(T);
};

template<unsigned N>
struct U2 {
  unsigned x : N;
};

template<typename T>
void f1(T) {
  struct Local {
    unsigned x : sizeof(T);
  };
}

template<unsigned N>
void f2() {
  struct Local {
    unsigned x : N;
  };
}
