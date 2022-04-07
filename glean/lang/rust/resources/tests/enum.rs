/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

pub enum Foo {
    Bar,
    Baz { a: i32, b: f32 },
    Boo(usize, isize),
}
