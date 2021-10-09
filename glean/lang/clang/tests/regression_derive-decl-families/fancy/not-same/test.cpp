// Copyright (c) Facebook, Inc. and its affiliates.

namespace A {

void f() {}
struct T {};

#include "a.h"

}

namespace B {

void f() {}
struct T {};

#include "a.h"

}
