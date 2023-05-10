/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

// grep like thing pg 432

use std::io;
use std::io::prelude::*;

fn grep<R>(target: &str, reader: R) -> io::Result<()>
where
    R: BufRead,
{
    // let stdin = io::stdin();

    // taking the lock is in the api of stdin. interesting. not hidden in an mvar.
    for res in reader.lines() {
        let l = res?;
        if l.contains(target) {
            println!("{}", l);
        }
    }
    Ok(())
}

pub fn main() {
    grep("for", io::stdin().lock()).unwrap();
}
