/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "glean/client/swift/ScubaLogger.h"
#include <glog/logging.h>
#include <cstdlib>

namespace facebook::glean::swift {

ScubaLogger::ScubaLogger()
    : unixname_(std::getenv("USER") ? std::getenv("USER") : "unknown"),
      hostmachine_(boost::asio::ip::host_name()) {}

void ScubaLogger::logRequest(
    facebook::rfe::ScubaData* scubaLogger,
    const std::string& method,
    const std::string& usr,
    USRType usrType,
    const std::string& result,
    Status status,
    int64_t duration,
    const std::string& error,
    const std::optional<std::string>& revision,
    const std::string& mode) {
  if (!scubaLogger) {
    return;
  }

  facebook::rfe::ScubaDataSample sample;
  sample.setTimeColumnNow();

  // Core request information
  sample.addNormalValue("method", method);
  sample.addNormalValue("usr", usr);
  sample.addNormalValue("usr_type", usrTypeToString(usrType));
  sample.addNormalValue("result", result);
  sample.addNormalValue("status", statusToString(status));
  sample.addNormalValue("error", error);

  // Environment information
  sample.addNormalValue("unixname", unixname_);
  sample.addNormalValue("hostmachine", hostmachine_);

  // Timing and status
  sample.addIntValue("time_taken_ms", duration);
  sample.addIntValue("result_size", static_cast<int64_t>(result.length()));
  sample.addIntValue("usr_length", static_cast<int64_t>(usr.length()));

  // Optional revision information
  if (revision.has_value()) {
    sample.addNormalValue("revision", revision.value());
  }

  // Mode information (production/test)
  sample.addNormalValue("mode", mode);

  // Add sample to scuba
  scubaLogger->addSample("swift_glass_client", sample);

  // Log to console for debugging
  LOG(INFO) << "Request - method: " << method
            << " usr_type: " << usrTypeToString(usrType)
            << " status: " << statusToString(status) << " time: " << duration
            << "ms" << " result_size: " << result.length()
            << (error.empty() ? "" : " error: " + error);
}

std::string ScubaLogger::usrTypeToString(USRType type) const {
  switch (type) {
    case USRType::SWIFT:
      return "swift";
    case USRType::CPP:
      return "cpp";
    case USRType::UNKNOWN:
      return "unknown";
    default:
      return "invalid";
  }
}

std::string ScubaLogger::statusToString(Status status) const {
  switch (status) {
    case Status::SUCCESS:
      return "success";
    case Status::FAILED:
      return "failed";
    case Status::TIMEOUT:
      return "timeout";
    case Status::NOT_FOUND:
      return "not_found";
    default:
      return "invalid";
  }
}

} // namespace facebook::glean::swift
