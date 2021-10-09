struct T {
  int m1;
  int m2 : 5;
  mutable int m3;

  void f() {
    m1;
    m2;
    m3;
  }
};

void g(T x, T *y) {
  x.m1;
  x.m2;
  x.m3;
  y->m1;
  y->m2;
  y->m3;
  &T::m1;
  &T::m3;
}
