// Copyright (c) Facebook, Inc. and its affiliates.

struct Foo {};
struct Bar {};
struct Baz {};

template<typename T>
struct S {
  Foo x;
  T y;
};

template<typename T>
struct S<T*> {
  Foo x;
  T* y;
};

S<Bar> x1;
S<Baz> x2;
S<Bar*> x3;
