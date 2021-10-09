namespace foo {

struct S {};
void f();

namespace bar {

struct T {};
void g();

}
}

void h() {
  foo::S s;
  foo::bar::T t;
  foo::f();
  foo::bar::g();
}
