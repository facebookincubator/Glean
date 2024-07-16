/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

template <typename T>
struct Base {};

template <typename T>
struct Middle : Base<T> {};

struct Derived : Middle<int> {};

template <typename T>
using AliasToBase = Base<T>;

template <typename T>
struct MiddleThroughAlias : AliasToBase<T> {};

using AliasToMiddle = MiddleThroughAlias<int>;

struct DerivedThroughAlias : AliasToMiddle {};
