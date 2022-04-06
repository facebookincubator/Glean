// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

pub trait Foo {
    fn bar(&self) -> i32;
}

impl Foo for i32 {
    fn bar(&self) -> i32 {
        0
    }
}
