// Copyright (c) Facebook, Inc. and its affiliates.

namespace N1 {

template<typename T> void f(T) {}
template<> void f<int>(int) {}
template void f<bool>(bool);

}

namespace N2 {
using namespace N1;

void g() {
  f("");
  f(0);
  f(true);
}
}

namespace N3 {

using N1::f;

void g() {
  f("");
  f(0);
  f(true);
}
}
