struct Ptr {
  void *value;
  operator bool() { return value; }
  operator int() { return 1; }
};

void bar(int);

struct Foo {
  Ptr ptr;

  void foo() {
    ptr;
    bar(ptr);
    if (ptr) {}
  }
};

void baz(Foo x) {
  x.ptr;
  bar(x.ptr);
  if (x.ptr) {}
}
