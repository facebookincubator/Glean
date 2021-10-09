// (c) Facebook, Inc. and its affiliates. Confidential and proprietary.

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