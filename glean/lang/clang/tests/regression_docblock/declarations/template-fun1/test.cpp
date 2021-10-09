// Copyright (c) Facebook, Inc. and its affiliates.

/**
 * @testname declarations/template-fun1
 * @type f T
 */
template<typename T> T f(T x) {
  return x;
}

/**
 * @testname declarations/template-fun1
 * @type f T*
 */
template<typename T> T* f(T* x) {
  return x;
}

/**
 * @testname declarations/template-fun1
 * @type g
 */
void g(int x) {
  f(x);
  f(&x);
  f("");
}
