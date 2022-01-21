/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

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
