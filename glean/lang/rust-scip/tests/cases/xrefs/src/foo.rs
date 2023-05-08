/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

// a module
// public type
#[derive(Debug)]
pub struct Foo {
    deep_foo: u64,
}

// public interface
pub fn new_foo(deep_foo: u64) -> Foo {
    new_secret_foo(deep_foo)
}

// not exported from the module
fn new_secret_foo(deep_foo: u64) -> Foo {
    Foo { deep_foo }
}

#[test]
fn main() {
    let foo = new_foo(42);
    println!("Foo: {:?}", foo);
}
