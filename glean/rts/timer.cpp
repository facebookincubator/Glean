// (c) Facebook, Inc. and its affiliates. Confidential and proprietary.

#include "glean/rts/timer.h"

namespace facebook::glean::rts {

void TimerLogger::operator()(
    folly::StringPiece msg,
    const std::chrono::duration<double>& sec) const {
  VLOG(1) << msg << " in "
          << prettyPrint(sec.count(), folly::PrettyType::PRETTY_TIME);
}

folly::AutoTimer<TimerLogger> makeAutoTimer(std::string&& msg) {
  return folly::makeAutoTimer<TimerLogger>(std::move(msg));
}

} // namespace facebook::glean::rts
