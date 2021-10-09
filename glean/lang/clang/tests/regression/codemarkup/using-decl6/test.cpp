namespace N1 {
enum Foo {X};
}

namespace N2 {
template<typename T> struct Bar {};
template<typename T> void bar() {}
}

namespace N3 {
using N1::Foo;
N2::Bar<Foo> x;
void f() {
  N2::bar<Foo>();
}
}
