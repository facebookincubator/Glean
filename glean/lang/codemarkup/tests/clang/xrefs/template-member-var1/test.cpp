/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

template<typename T, typename U> struct X {};

template<typename T> struct A {
  static int foo;
  static T bar;
  template<typename U> static X<T,U> baz;
};

template<> struct A<int> {
  static int foo;
  static int bar;
  template<typename U> static X<int,U> baz;
};

/*
template<typename T>
int A<T*>::foo;
*/

template<typename T>
template<typename U>
X<T,U*> A<T>::baz<U*>;

template<typename T> int A<T>::foo;
template<typename T> T A<T>::bar;
template<typename T> template<typename U> X<T,U> A<T>::baz;

template<>
char* A<char*>::bar;

int A<int>::foo;
int A<int>::bar;
template<typename U> X<int,U> A<int>::baz;

void f() {
  A<double>::foo;
  A<double>::bar;
  A<double>::baz<void>;
  A<double>::baz<int*>;
  A<double>::baz<char*>;

  A<char*>::foo;
  A<char*>::bar;
  A<char*>::baz<void>;
  A<char*>::baz<int*>;
  A<char*>::baz<char*>;

  A<int>::foo;
  A<int>::bar;
  A<int>::baz<void>;
  A<int>::baz<int*>;
  A<int>::baz<char*>;
}
