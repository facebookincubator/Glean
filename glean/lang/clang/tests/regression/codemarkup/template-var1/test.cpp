/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

template<typename T> constexpr int foo = 1;
template<typename T> constexpr int foo<T*> = 2;
template<> constexpr int foo<bool> = 3;
template constexpr int foo<void>;

void h() {
  foo<int>;
  foo<int*>;
  foo<bool>;
  foo<void>;
}
