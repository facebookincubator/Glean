/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

// rustc -C panic=abbort panic.rs
#[allow(unreachable_code)]
pub fn main() {
    // unwinding
    {
        println!("{}", pirate_share(100, 15));
        println!("{}", pirate_share(100, 1));
    }

    panic!("{:?}", panic!()); // explicit panic.

    // interesting:
    fix();

    {
        println!("{}", pirate_share(100, 0)); // panicked at 'attempt to divide by zero'
    }
}

fn fix() -> ! {
    loop {
        panic!()
    }
}

fn pirate_share(total: u64, crew_size: usize) -> u64 {
    (total / 2) / crew_size as u64
}
