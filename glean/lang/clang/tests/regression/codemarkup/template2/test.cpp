template<typename T> struct A {
  template<typename U> static void f(U);
};

template<>
template<>
void A<int*>::f<int>(int) {}

void g() {
  A<int>::f(0);
}
