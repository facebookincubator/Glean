// Copyright (c) Facebook, Inc. and its affiliates.

int foo(int x) {
  return 0;
}

int bar(int x) {
  return 1 + foo(x);
}

int main(int x, char** _unused) {
  return bar(x) *  2;
}
