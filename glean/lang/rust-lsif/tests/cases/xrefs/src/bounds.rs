/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::ops::Add;
use std::ops::Mul;

// num-generic dot product
fn dot<N>(v1: &[N], v2: &[N]) -> N
where
    N: Copy + Mul<Output = N> + Add<Output = N> + Default,
{
    let mut total: N = N::default();
    for i in 0..v1.len() {
        total = total + v1[i] * v2[i];
    }
    total
}

pub fn main() {
    let a: &[f64] = &[1., 2., 3., 4., 5.];
    let b: &[f64] = &[2., 7., 9., 3., 1.];

    println!("{}", dot(&a, &b));
}
