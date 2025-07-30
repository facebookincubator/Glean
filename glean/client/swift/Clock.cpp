/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "glean/client/swift/Clock.h"

namespace facebook::glean::swift {

Clock::Clock() : start_(std::chrono::high_resolution_clock::now()) {}

int64_t Clock::duration() const {
  auto now = std::chrono::high_resolution_clock::now();
  return std::chrono::duration_cast<std::chrono::milliseconds>(now - start_)
      .count();
}

} // namespace facebook::glean::swift
