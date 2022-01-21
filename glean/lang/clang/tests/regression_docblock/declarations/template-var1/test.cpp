/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

/**
 * @testname declarations/template-var1
 * @struct U1
 */
template<typename T>
struct U1 {
  /**
  * @testname declarations/template-var1
  * @type U1::x
  */
  unsigned x : sizeof(T);
};

/**
 * @testname declarations/template-var1
 * @struct U2
 */
template<unsigned N>
struct U2 {
  /**
  * @testname declarations/template-var1
  * @type U2::x
  */
  unsigned x : N;
};

/**
 * @testname declarations/template-var1
 * @fun f1
 */
template<typename T>
void f1(T) {
  /**
  * @testname declarations/template-var1
  * @type f1::Local
  */
  struct Local {
    /**
    * @testname declarations/template-var1
    * @type f1::Local::x
    */
    unsigned x : sizeof(T);
  };
}

/**
 * @testname declarations/template-var1
 * @fun f2
 */
template<unsigned N>
void f2() {
  /**
  * @testname declarations/template-var1
  * @type f2::Local
  */
  struct Local {
    /**
    * @testname declarations/template-var1
    * @type f2::Local::x
    */
    unsigned x : N;
  };
}
