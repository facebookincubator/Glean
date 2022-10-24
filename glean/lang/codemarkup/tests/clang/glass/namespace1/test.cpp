/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

 #include "test.h"

namespace foo {

struct S {
  typedef long TypedefDeclInteger;
  using UsingDeclInteger = long;
};

/**
 * Test Code Block
 */
void f();

/* test/cpp/foo/foo */
void foo() {
  int four = FOO(2, 2);
  return;
}

/* check duplicate symbols in same namespace */
/* test/cpp/foo/foo */
void foo();

namespace bar {

typedef short TypedefDeclShort;
using UsingDeclShort = short;

struct T {
  int memberVariableDecl;
};
void g();

}

int fooFn(int fooI) {
  return fooI;
}

}

// add decl occurrence
void h(int _i);

void h(int i) {
  ::h(i);
  foo::S s;
  foo::bar::T t;
  foo::f();
  foo::foo();
  foo::bar::g();
  i++;
}

namespace maybe {

/**
 * Structs and their constructors
 */
struct Nothing {
  enum class _secret { _magic };

  explicit constexpr Nothing(_secret) {}
};
constexpr Nothing nothing{Nothing::_secret::_magic};

struct Something {
  int a_;
  Something(int a): a_(a){}
  ~Something() {}
};


}

#include "test.h"

#define A "one"
#define B "two"

namespace baz {

struct U {
  enum InU { UA, UB, UC };
  enum class InUClass { UCA, UCB, UCC };
};

}

enum Global { GA, GB, GC };
enum class GlobalClass { GCA, GCB, GCC };
