// Copyright (c) Facebook, Inc. and its affiliates.

#define IFDEF1
#define IFDEF2

#ifdef IFDEF1
int f1() {
  return 1;
}
#endif

#ifndef IFDEF2
#error "ERROR!"
#else
void f2() {
  return;
}
#endif
