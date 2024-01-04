/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <coroutine>

template <typename T>
struct Task;

struct VoidPromise {
  Task<void> get_return_object() noexcept;
  std::suspend_never initial_suspend() const noexcept;
  std::suspend_never final_suspend() const noexcept;
  void return_void() {}
  void unhandled_exception() {}
};

template <typename T>
struct ValuePromise {
  Task<int> get_return_object() noexcept;
  std::suspend_never initial_suspend() const noexcept;
  std::suspend_never final_suspend() const noexcept;
  void return_value(T) {}
  void unhandled_exception() {}
};

template <>
struct Task<void> {
  using promise_type = VoidPromise;
};

template <typename T>
struct Task {
  using promise_type = ValuePromise<T>;
};

Task<void> f() {
  co_return;
}

Task<int> g() {
  int x = 42;
  co_return x;
}
