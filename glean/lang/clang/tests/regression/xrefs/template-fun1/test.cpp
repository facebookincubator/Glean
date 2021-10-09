// Copyright (c) Facebook, Inc. and its affiliates.

struct Foo { void mem(); };
struct Bar { void mem(); };
struct Baz { void mem(); };

void global();
void apply(Foo);
void apply(Bar);
void apply(Baz);

template<typename T> void foo(T x) {
  global();
  apply(x);
  x.mem();
}

template<> void foo<>(Foo* x) {
  x->mem();
  apply(*x);
  global();
}

template void foo<Baz>(Baz);

void h() {
  Foo x;
  Bar y;
  Baz z;
  foo(x);
  foo(y);
  foo(&x);
  foo(z);
}
