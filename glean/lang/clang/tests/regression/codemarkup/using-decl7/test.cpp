namespace N1 {
template<typename T> void foo() {}
}

using N1::foo;

template<typename T> void bar() {
  foo<T>();
}

namespace N2 {
void foo(int);
}

namespace N3 {
void foo(bool);
}

namespace N4 {
using N2::foo;
using N3::foo;

template<typename T> void bar(T x) {
  foo(x);
}

}
