/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

namespace N1 {

template<typename T> struct Foo {};
template<template<typename> class C> struct Bar {};

void f1() {
  Bar<Foo> x;
}

}

void f2() {
  using N1::Foo;
  N1::Bar<Foo> x;
}

void f3() {
  using namespace N1;
  Bar<Foo> x;
}
