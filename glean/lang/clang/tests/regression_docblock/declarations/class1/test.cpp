/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

/** Ensure this offset=0 comment is in the output
 *
 * triple slash comments for struct T
 * @testname declarations/class1
 * @struct T
 */
struct T {};

/** Slash star star comments for struct U
 * @struct U
 * @testname declarations/class1
 */
struct U {
  U();
  U(const U&);
  U& operator=(const U&);
};

/// slash slash bang comments for struct A
/// @testname declarations/class1
/// @struct A
struct A {
  virtual ~A() = 0;
};

/*! Slash star bang comments for struct U
 * @struct U
 * @testname declarations/class1
 */
struct B : A {
  U u;
};
