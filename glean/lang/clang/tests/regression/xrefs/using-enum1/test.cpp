// Copyright (c) Facebook, Inc. and its affiliates.

namespace N1 {

enum X { X1, X2, X3 };
enum class Y { Y1, Y2, Y3 };

}

void foo() {
  using namespace N1;

  X1;
  Y::Y1;
}

void bar () {
  using N1::X1;
  using N1::Y;

  X1;
  Y::Y1;
}
