// Copyright (c) Facebook, Inc. and its affiliates.

struct Foo {
  Foo();
  Foo(const Foo&);
  ~Foo();

  operator bool();
  operator const unsigned int *();
  void operator=(Foo&);
  void operator+(Foo&);
  void operator--();
};

bool operator ""_foo(const char *);
