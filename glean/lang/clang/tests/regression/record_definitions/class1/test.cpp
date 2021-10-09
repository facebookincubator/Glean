// Copyright (c) Facebook, Inc. and its affiliates.

struct T {};
struct U {
  U();
  U(const U&);
  U& operator=(const U&);
};

struct A {
  virtual ~A() = 0;
};

struct B : A {
  U u;
  int lucky() { return 0; };

  int varB;
};
