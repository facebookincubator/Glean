/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include <boost/asio/ip/host_name.hpp>
#include <optional>
#include <string>
#include "rfe/scubadata/ScubaData.h"

namespace facebook::glean::swift {

enum class USRType { SWIFT, CPP, UNKNOWN };

enum class Status { SUCCESS, FAILED, TIMEOUT, NOT_FOUND };

class ScubaLogger {
 public:
  explicit ScubaLogger();
  virtual ~ScubaLogger() = default;

  virtual void logRequest(
      facebook::rfe::ScubaData* scubaLogger,
      const std::string& method,
      const std::string& usr,
      USRType usrType,
      const std::string& result,
      Status status,
      int64_t duration,
      const std::string& error = "",
      const std::optional<std::string>& revision = std::nullopt);

 private:
  const std::string unixname_;
  const std::string hostmachine_;

  std::string usrTypeToString(USRType type) const;
  std::string statusToString(Status status) const;
};

} // namespace facebook::glean::swift
