/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

pub fn add2(a: i32, b: i32) -> i32 {
    let sum = a + b;
    sum
}

pub fn add3(a: i32, b: i32, c: i32) -> i32 {
    add2(add2(a, b), c)
}
