/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

template<typename T> struct A {
  static void foo();
  static void bar(T);
  template<typename U> static void baz(T,U);
};

template<> struct A<int> {
  static void foo();
  static void bar(int);
  template<typename U> static void baz(int,U);
};

template<>
void A<bool>::foo() {}

template<>
template<>
void A<bool>::baz<bool>(bool,bool) {}

void f() {
  A<double>::foo();
  A<double>::bar(0);
  A<double>::baz(0,"");
  A<double>::baz(0,true);

  A<int>::foo();
  A<int>::bar(0);
  A<int>::baz(0,"");
  A<int>::baz(0,true);

  A<bool>::foo();
  A<bool>::bar(true);
  A<bool>::baz(true,"");
  A<bool>::baz(true,true);
}
