// Copyright (c) Facebook, Inc. and its affiliates.

#define FOO foo
#define REF(x) x
#define PASTE(x,y) x##y

#define NEST_FOO FOO
#define NEST_REF(x) REF(x)

#define N_FOO ::N::foo()
#define N_BAR(x) ::N::bar(x)

namespace N {
  void foo() {}
  void bar(int) {}
  int var;
}

struct Foo {
  void foo() {}
  void bar(int) {}
  int field;
};

void f() {
  using namespace N;

  N::FOO();
  N::REF(foo)();
  N::PASTE(f,oo)();
  N::REF(bar)(N::REF(var));
  N::PASTE(b,ar)(N::PASTE(v,ar));
  N::NEST_FOO();
  N::NEST_REF(foo)();

  FOO();
  REF(foo)();
  PASTE(f,oo)();
  REF(bar)(REF(var));
  PASTE(b,ar)(PASTE(v,ar));
  NEST_FOO();
  NEST_REF(foo)();

  REF(N::foo)();
  REF(N::bar)(REF(N::var));

  N_FOO;
  N_BAR(N::var);
  N_BAR(N::REF(var));

  Foo x;

  x.FOO();
  x.REF(foo)();
  x.PASTE(f,oo)();
  x.NEST_FOO();
  x.NEST_REF(field);
  x.REF(bar)(x.REF(field));
}
