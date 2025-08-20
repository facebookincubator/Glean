/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include <folly/dynamic.h>
#include <gtest/gtest_prod.h>
#include <atomic>
#include <iostream>
#include <memory>
#include "glean/client/swift/IGlassAccess.h"
#include "glean/client/swift/ScubaLogger.h"
#include "rfe/scubadata/ScubaData.h"

class JsonServer {
 public:
  JsonServer();
  ~JsonServer() = default;
  JsonServer(const JsonServer&) = delete;
  JsonServer& operator=(const JsonServer&) = delete;
  JsonServer(JsonServer&&) = delete;
  JsonServer& operator=(JsonServer&&) = delete;

  void setGlassAccess(std::unique_ptr<IGlassAccess> glassAccess);
  void setScubaLogger(
      std::unique_ptr<facebook::glean::swift::ScubaLogger> scubaLogger);
  void start(std::istream& input = std::cin, std::ostream& output = std::cout);
  void stop();

 private:
  void processRequest(const std::string& requestStr, std::ostream& output);
  FRIEND_TEST(JsonServerTest, USRToDefinitionRequest);
  FRIEND_TEST(JsonServerTest, USRToDefinitionRequestNoResults);
  FRIEND_TEST(JsonServerTest, UnknownMethodRequest);
  FRIEND_TEST(JsonServerTest, InvalidJSONRequest);
  std::atomic<bool> running_;
  std::unique_ptr<IGlassAccess> glassAccess_;
  std::unique_ptr<facebook::glean::swift::ScubaLogger> scubaLogger_;
  std::unique_ptr<facebook::rfe::ScubaData> scubaData_;

  void handleUSRToDefinitionRequest(
      const folly::dynamic& id,
      const std::string& usr,
      const std::optional<std::string>& revision,
      const std::string& mode,
      std::ostream& output);
  void sendErrorResponse(
      const folly::dynamic& id,
      int code,
      const std::string& message,
      std::ostream& output,
      const std::optional<std::string>& method = std::nullopt,
      const std::optional<std::string>& usr = std::nullopt,
      const std::optional<facebook::glean::swift::USRType>& usrType =
          std::nullopt,
      int64_t duration = 0,
      const std::string& mode = "production");

  facebook::glean::swift::USRType determineUSRType(const std::string& usr);
};
