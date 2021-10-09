// Copyright (c) Facebook, Inc. and its affiliates.

namespace N1 {

struct T {
  using U = int;
  static void f();
};
enum E { E1, E2, E3 };
enum class F { F1, F2, F3 };

using U = int;
typedef bool V;

template<typename T>
struct W { using Type = T; static int f(); };

}

namespace N2 {

using N1::T;
using N1::E;
using N1::E1;
using N1::F;
using N1::U;
using N1::V;
using N1::W;

T x1;
E x2 = E1;
F x3 = F::F2;
U x4;
V x5;
W<int> x6;
W<double>::Type x7;
int x8 = W<double>::f();

void foo() {

T x1;
E x2 = E1;
F x3 = F::F2;
U x4;
V x5;
W<int> x6;
W<double>::Type x7;
int x8 = W<double>::f();

}

}

void foo() {

using N1::T;
using N1::E;
using N1::E1;
using N1::F;
using N1::U;
using N1::V;
using N1::W;

T x1;
E x2 = E1;
F x3 = F::F2;
U x4;
V x5;
W<int> x6;
W<double>::Type x7;
int x8 = W<double>::f();

}
