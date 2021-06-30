// Copyright (c) Facebook, Inc. and its affiliates.

#include "test.h"

namespace facebook {

int global = 3;

void f(int x) {}

C::C() { a = 0; }
C::C(int x) : a(x) { }

C::~C() {}

int C::get() { return a; }

}

using namespace facebook;

int main() {
  C c { global };

  return TEST(c.get());
}
