/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

// Stands in for <cstdint> and the OSF runtime headers. The OSF headers
// are not on this test's include path, and no system header is included,
// so facts do not depend on the C++ library version.

#pragma once

using int16_t = short;
using int32_t = int;
using uint64_t = unsigned long;

namespace arfx::osf {

class IService {
 public:
  virtual ~IService() = default;
};

template <typename T>
class FutureStatusOr {};

} // namespace arfx::osf
