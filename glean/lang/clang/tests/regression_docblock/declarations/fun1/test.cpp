/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

/** Ensure this offset=0 comment is in the output
 *
 * @testname declarations/fun1
 * @type void f1
 */
void f1();
/**
 * @testname declarations/fun1
 * @type void f2
 */
void f2(bool x, int y);
/**
 * @testname declarations/fun1
 * @type void f3
 */
void f3(int, char, char);
/**
 * @testname declarations/fun1
 * @type int f4
 */
int f4();
/**
 * @testname declarations/fun1
 * @type static void f5
 */
static void f5();
