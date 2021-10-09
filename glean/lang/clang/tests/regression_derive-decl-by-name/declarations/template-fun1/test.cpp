// Copyright (c) Facebook, Inc. and its affiliates.

template<typename T> T f(T x) {
  return x;
}

template<typename T> T* f(T* x) {
  return x;
}

void g(int x) {
  f(x);
  f(&x);
  f("");
}
