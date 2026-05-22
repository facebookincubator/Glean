/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include <fmt/core.h>
#include <string>

namespace facebook {
namespace glean {
namespace rts {

[[noreturn]] void raiseError(const std::string& msg);

[[noreturn]] inline std::string error(const std::string& msg) {
  raiseError(msg);
}

template <class... Args>
[[noreturn]]
inline std::string error(fmt::format_string<Args...> fmt, Args&&... args) {
  raiseError(fmt::format(fmt, std::forward<Args>(args)...));
}

} // namespace rts
} // namespace glean
} // namespace facebook
