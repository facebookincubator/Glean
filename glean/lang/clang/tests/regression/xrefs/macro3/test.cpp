// Copyright (c) Facebook, Inc. and its affiliates.

#define PASTE(x,y) x##y
#define NESTED_PASTE(x,y) PASTE(x,y)

#define MACRO_1(x) x
#define MACRO_2(x) MACRO_1(x)
#define MACRO_3(x) MACRO_2(x)

void foobar() {}

void f() {
  MACRO_1(PASTE(foo,bar))();
  MACRO_2(PASTE(foo,bar))();
  MACRO_3(PASTE(foo,bar))();

  NESTED_PASTE(foo,bar)();
}
