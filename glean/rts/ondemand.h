/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include <atomic>
#include <memory>

namespace facebook {
namespace glean {
namespace rts {

/// Values which are created once when needed. OnDemand is thread safe and
/// always returns the same value. However, it might speculatively create (and
/// then destroy) additional values.
template<typename T>
class OnDemand final {
public:
  /// Create an empty OnDemand
  OnDemand() noexcept : ptr_(nullptr) {}

  OnDemand(OnDemand&& other) noexcept
    : ptr_(other.ptr_.exchange(nullptr, std::memory_order_acq_rel))
    {}

  OnDemand& operator=(OnDemand&& other) noexcept {
    auto p = other.ptr_.exchange(nullptr, std::memory_order_acq_rel);
    p = ptr_.exchange(ptr_, std::memory_order_release);
    delete p;
    return *this;
  }

  ~OnDemand() noexcept {
    delete ptr_.load(std::memory_order_relaxed);
  }

  OnDemand(const OnDemand&) = delete;
  OnDemand& operator=(const OnDemand&) = delete;

  /// Get the value, creating it if necessary.
  T& value() {
    auto p = ptr_.load(std::memory_order_relaxed);
    if (p == nullptr) {
      auto k = new T();
      if (ptr_.compare_exchange_strong(p, k, std::memory_order_acq_rel)) {
        p = k;
      } else {
        delete k;
      }
    }
    return *p; 
  }

  const T& value() const {
    return const_cast<OnDemand*>(this)->get();
  }

private:
  mutable std::atomic<T*> ptr_;
};

}
}
}
