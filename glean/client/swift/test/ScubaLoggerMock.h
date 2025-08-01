/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include <gmock/gmock.h>
#include "glean/client/swift/ScubaLogger.h"

namespace facebook::glean::swift {

class ScubaLoggerMock : public ScubaLogger {
 public:
  mutable int logRequestCallCount = 0;
  mutable std::string lastMethod;
  mutable std::string lastUsr;
  mutable USRType lastUsrType{USRType::UNKNOWN};
  mutable std::string lastResult;
  mutable Status lastStatus{Status::FAILED};
  mutable int64_t lastDuration{};
  mutable std::string lastError;
  mutable std::optional<std::string> lastRevision;
  mutable std::string lastMode;

  void logRequest(
      facebook::rfe::ScubaData* scubaLogger,
      const std::string& method,
      const std::string& usr,
      USRType usrType,
      const std::string& result,
      Status status,
      int64_t duration,
      const std::string& error = "",
      const std::optional<std::string>& revision = std::nullopt,
      const std::string& mode = "production") override {
    (void)scubaLogger; // Suppress unused parameter warning
    // Don't actually log to Scuba, just record the call
    logRequestCallCount++;
    lastMethod = method;
    lastUsr = usr;
    lastUsrType = usrType;
    lastResult = result;
    lastStatus = status;
    lastDuration = duration;
    lastError = error;
    lastRevision = revision;
    lastMode = mode;
  }

  // Helper methods for testing
  void expectLogRequest(
      const std::string& expectedMethod,
      const std::string& expectedUsr,
      USRType expectedUsrType,
      Status expectedStatus,
      const std::string& expectedError = "",
      const std::string& expectedMode = "production") const {
    EXPECT_EQ(1, logRequestCallCount);
    EXPECT_EQ(expectedMethod, lastMethod);
    EXPECT_EQ(expectedUsr, lastUsr);
    EXPECT_EQ(expectedUsrType, lastUsrType);
    EXPECT_EQ(expectedStatus, lastStatus);
    EXPECT_EQ(expectedError, lastError);
    EXPECT_EQ(expectedMode, lastMode);
  }

  void reset() {
    logRequestCallCount = 0;
    lastMethod.clear();
    lastUsr.clear();
    lastUsrType = USRType::UNKNOWN;
    lastResult.clear();
    lastStatus = Status::FAILED;
    lastDuration = 0;
    lastError.clear();
    lastRevision = std::nullopt;
    lastMode.clear();
  }
};

} // namespace facebook::glean::swift
