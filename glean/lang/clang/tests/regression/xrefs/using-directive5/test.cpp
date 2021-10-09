// Copyright (c) Facebook, Inc. and its affiliates.

namespace N1 {

struct T {};

}

using N1::T;

namespace N2 {
namespace N21 {

using namespace N2;

void foo() {
  T x;
}

}
}
