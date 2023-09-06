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

template<
    class T1,
    class T2
> struct pear;

struct d {

 int i;

 public:
    /**
     * Default constructor, initializes with nullptr.
     */
    d();

    d(char const* val);

    d(char const* val, void (*)());

    d(char c, int i, char const* p);

    d(d const&);

    d(pear<int, bool>& p, char cp);

};

namespace Q {

  void bozzo() { return; }

  struct QQ {

    void bozzo();

    double getDouble() const&;
    double& getDouble() &;
    double getDouble() &&;

    int thango(int, QQ const&) const&;
    int& thango(int, QQ&) &;
    int thango(int, QQ&&) &&;

    int thingo(pear<int,char>, QQ&&) const;

    ~QQ() noexcept;
  };

  struct OO {

    friend bool operator==(OO const& a, OO const& b);

    OO& operator++();

    OO& operator=(OO const&);
    OO& operator=(OO&&) noexcept;

    template <typename T>
    constexpr operator T&() const&&;
    template <typename T>
    constexpr operator T&() const&;

  };

  constexpr void operator"" _glean(
      char32_t const* str, unsigned long len) noexcept {
    return;
  }

}

namespace N1 {

namespace N2 {

namespace N3 { struct Foo {}; }

using N3::Foo;


// A function taking 'Foo x' and returning a pointer to a function that takes
// 'Foo y' and returns 'Foo';
Foo (*foo(Foo x))(Foo y);

struct Bar {
  // Same, except as a member
  Foo (*bar(Foo x))(Foo y);
};

}

}

using N1::N2::N3::Foo;

Foo id(Foo x);

// The 'Foo' in 'Foo x' should go through the using decl in N2, the other 'Foo'
// through the top level one
Foo (*N1::N2::foo(Foo x))(Foo y) {
  return &id;
}

Foo (*N1::N2::Bar::bar(Foo x))(Foo y) {
  return &id;
}
