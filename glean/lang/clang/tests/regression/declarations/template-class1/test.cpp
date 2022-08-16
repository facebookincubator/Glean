/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

template <typename T>
struct S;

template <typename U>
struct S {
  S(const S&) {}
  S(S&&) {}

  operator U() const {
    return {};
  }
};
