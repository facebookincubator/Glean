/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

// Interior mutability

use std::cell::RefCell;
use std::fs::File;
use std::mem::MaybeUninit;

#[derive(Debug)]
pub struct Spider {
    species: String,
    web: bool,
    legs: [File; 8],
    err_count: RefCell<u32>,
}

fn mk_spider() -> Result<Spider, std::io::Error> {
    let fs: [File; 8] = {
        let mut fs: [MaybeUninit<File>; 8] = unsafe { MaybeUninit::uninit().assume_init() };

        for (i, p) in fs.iter_mut().enumerate() {
            let f = File::create(format!("/tmp/f-{}.txt", i))?;
            *p = MaybeUninit::new(f);
        }

        // now convert
        unsafe { std::mem::transmute::<_, [File; 8]>(fs) }
    };

    Ok(Spider {
        species: "scary".to_string(),
        web: false,
        legs: fs,
        err_count: RefCell::new(0),
    })
}

pub fn main() {
    let spider = mk_spider().expect("Unable to create spider!");
    println!("{:?}", spider);
}
