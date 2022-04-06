// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

pub fn add2(a: i32, b: i32) -> i32 {
    let sum = a + b;
    sum
}

pub fn add3(a: i32, b: i32, c: i32) -> i32 {
    add2(add2(a, b), c)
}
