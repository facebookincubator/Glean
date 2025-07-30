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

class JsonServer {
 public:
  JsonServer();
  ~JsonServer() = default;
  JsonServer(const JsonServer&) = delete;
  JsonServer& operator=(const JsonServer&) = delete;
  JsonServer(JsonServer&&) = delete;
  JsonServer& operator=(JsonServer&&) = delete;

  void setGlassAccess(std::unique_ptr<IGlassAccess> glassAccess);
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
  void handleUSRToDefinitionRequest(
      const folly::dynamic& id,
      const std::string& usr,
      std::ostream& output);
  void sendErrorResponse(
      const folly::dynamic& id,
      int code,
      const std::string& message,
      std::ostream& output);
};
