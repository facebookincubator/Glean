namespace N1 {

namespace N2 {

struct XXX {}; // visible to N2 definitions

namespace N3 { struct YYY {}; }
namespace N4 { void callme(); }

using N3::YYY; // visible to N2 definitions
using namespace N4;

struct Foo {
  struct Bar {}; // visible to Foo definitions
  Bar foo(Foo, Bar);
  void bar(XXX, YYY);
};

void standalone(Foo::Bar, YYY);

}

using N2::Foo;

}

namespace M1 { struct ZZZ {}; }

using namespace N1; // brings Foo into scope via the using decl in N1
using namespace M1; // brings ZZZ into scope

Foo::Bar Foo::foo(Foo, Bar) {
  XXX x; // visible in N2
  YYY y; // visible in N2 via using
  Foo z; // visible in Foo, shouldn't go through any using decls
  callme(); // visible in N2 via using namespace
  return {};
}

void Foo::bar(XXX, YYY) {
  XXX x; // visible in N2
  YYY y; // visible in N2 via using
  Foo z; // visible in Foo, shouldn't go through any using decls
  callme(); // visible in N2 via using namespace
}

void N1::N2::standalone(Foo::Bar, YYY) {
  XXX x; // visible in N2
  YYY y; // visible in N2 via using
  Foo z; // visible in Foo, shouldn't go through any using decls
  callme(); // visible in N2 via using namespace
}
