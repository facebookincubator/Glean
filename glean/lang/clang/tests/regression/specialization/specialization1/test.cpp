/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

// cxx1.SpecializationOf is emitted only for AUTHOR-WRITTEN explicit/partial
// specializations, never for implicit instantiations. This fixture mixes both:
// the goldens must contain a fact for each spec below and NOTHING for the
// instantiation sites at the bottom.

// --- Class template: primary + explicit full spec + partial spec ---
template <typename T>
struct S {
  T value;
};

template <>
struct S<int> {
  int value;
};

template <typename T>
struct S<T*> {
  T* value;
};

// --- Function template: primary + explicit full spec ---
template <typename T>
T f(T x) {
  return x;
}

template <>
int f<int>(int x) {
  return x;
}

// --- Variable template: primary + explicit full spec ---
template <typename T>
T v = T{};

template <>
int v<int> = 5;

// --- Member function template: primary + explicit full spec ---
struct C {
  template <typename T>
  T m(T x);
};

template <typename T>
T C::m(T x) {
  return x;
}

template <>
int C::m<int>(int x) {
  return x;
}

// --- Instantiation sites: these are NOT specializations, so they must emit
// NO cxx1.SpecializationOf facts even though they appear in the TU. ---
S<double> instImplicitPrimary;  // implicit instantiation of primary S
S<char*> instImplicitPartial;   // implicit instantiation of the S<T*> partial
int useFn = f<char>('x');       // implicit instantiation of primary f
int useVar = v<char>;           // implicit instantiation of primary v
