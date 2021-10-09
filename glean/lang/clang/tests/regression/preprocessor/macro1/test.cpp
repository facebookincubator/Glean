// Copyright (c) Facebook, Inc. and its affiliates.

#define M1 int

M1 a1;
M1 a2;

#define ABCD(s) M1 s

ABCD(b1);
ABCD(b2);

#define HELLOHELLO(u) struct u { \
  ABCD(x1); \
  ABCD(x2); \
  M1 y1; \
}

HELLOHELLO(T);
HELLOHELLO(foobar);

#define FOOBAR(x,y,z) struct x { \
  y; \
  z; \
  M1 xxx; \
  ABCD(yyy); \
}

FOOBAR(a1, M1 x, ABCD(y));
FOOBAR(xyz, HELLOHELLO(a) x, HELLOHELLO(b) y);

#define XYZ M1

XYZ xyz1;

#define ZYX ABCD(zyx1)

ZYX;
