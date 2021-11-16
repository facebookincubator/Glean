/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

struct Foo {
  Foo();
  Foo(const Foo&);
  ~Foo();

  operator bool();
  operator const unsigned int *();
  void operator=(Foo&);
  void operator+(Foo&);
  void operator--();
};

bool operator ""_foo(const char *);
