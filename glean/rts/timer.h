/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include <folly/experimental/AutoTimer.h>

namespace facebook {
namespace glean {
namespace rts {

struct TimerLogger {
  void operator()(
      folly::StringPiece msg,
      const std::chrono::duration<double>& sec) const;
};

folly::AutoTimer<TimerLogger> makeAutoTimer(std::string&& msg);

} // namespace rts
} // namespace glean
} // namespace facebook
