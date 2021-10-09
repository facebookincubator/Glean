// Copyright (c) Facebook, Inc. and its affiliates.

template<typename T> struct A {
  void foo();
  void bar(T);
  template<typename U> void baz(T,U);
};

template<> struct A<int> {
  void foo();
  void bar(int);
  template<typename U> void baz(int,U);
};

template<>
void A<bool>::foo() {}

template<>
template<>
void A<bool>::baz<bool>(bool,bool) {}

void f() {
  A<double> x;
  A<int> y;
  A<bool> z;

  x.foo();
  x.bar(0);
  x.baz(0,"");
  x.baz(0,true);

  y.foo();
  y.bar(0);
  y.baz(0,"");
  y.baz(0,true);

  z.foo();
  z.bar(true);
  z.baz(true,"");
  z.baz(true,true);
}
