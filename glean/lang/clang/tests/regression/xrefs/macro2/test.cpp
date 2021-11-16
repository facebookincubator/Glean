/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#define BLAH(a, b, c, d)    \
    if (a == b) {           \
        c;                  \
    } else {                \
        d;                  \
    }                       \

int one() {
    return 1;
}

int two() {
    return 2;
}

int someFunc() {
    BLAH(
        one(),
        two(),
        return 1,
        return 2
    )
}
