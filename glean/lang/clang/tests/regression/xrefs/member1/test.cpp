/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

struct Ptr {
  explicit Ptr() = default;
  void *value;
  operator bool() { return value; }
  operator int() { return 1; }
};

void bar(int);

struct Foo {
  Ptr ptr;

  void foo() {
    ptr;
    bar(ptr);
    if (ptr) {}
  }
};

void baz(Foo x) {
  x.ptr;
  Ptr ptr;
  bar(x.ptr);
  if (x.ptr) {}
}
