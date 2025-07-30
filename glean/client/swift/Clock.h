/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include <chrono>

namespace facebook::glean::swift {

class Clock {
 public:
  Clock();
  ~Clock() = default;

  // Returns the duration in milliseconds since the Clock was created
  int64_t duration() const;

 private:
  std::chrono::high_resolution_clock::time_point start_;
};

} // namespace facebook::glean::swift
