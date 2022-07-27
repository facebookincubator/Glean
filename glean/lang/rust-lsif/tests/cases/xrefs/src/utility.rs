/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

// ch 282 / utility traits

struct A {
    name: String,
    nick: Vec<String>,
}

impl Drop for A {
    // run before dropping the associated memory
    fn drop(&mut self) {
        eprintln!("Dropping {}", self.name);
        if !self.nick.is_empty() {
            eprintln!("AKA ({})", self.nick.join(", "));
        }
    }
}

pub fn main() {
    let mut a = A {
        name: "Zeus".to_string(),
        nick: vec!["cloud man".to_string()],
    };
    println!("before");
    a = A {
        name: "B".to_string(),
        nick: vec![],
    };
    println!("after");
}
