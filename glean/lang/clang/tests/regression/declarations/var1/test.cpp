// Copyright (c) Facebook, Inc. and its affiliates.

extern int v1;
int v1;
static int v2;
inline int v3;
constexpr int v4 = 4;

struct T {
  static int m1;
  static int m2;
  inline static int m3;
  constexpr static int m4 = 4;
};

int T::m1;
inline int T::m2;

void f(int x1) {
  static int l1; // shouldn't be indexed
  int l2; // shouldn't be indexed
  [t=l2] {};
}
