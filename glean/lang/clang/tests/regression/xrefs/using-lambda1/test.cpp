// Copyright (c) Facebook, Inc. and its affiliates.

namespace N1 {

inline void foo() {}

}

void g() {
  auto bar = [&]() {
    using namespace N1;
    foo();
  };
  bar();
}

void h() {
  auto bar = [&](auto) {
    using namespace N1;
    foo();
  };
  bar(1);
  bar("");
}
