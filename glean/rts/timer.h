// (c) Facebook, Inc. and its affiliates. Confidential and proprietary.

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
