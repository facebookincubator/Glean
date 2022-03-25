/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

template<typename T>
struct S {};

template<typename T> using F1 = T;
template<typename T> using F2 = int;
template<typename T> using F3 = S<T>;

F1<int> x;
F2<int> y;
F3<int> z;
