/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "glean/client/swift/JsonServer.h"
#include <gtest/gtest.h>
#include <chrono>
#include <sstream>
#include <thread>

class JsonServerTest : public ::testing::Test {
 protected:
  void SetUp() override {
    // Reset server state before each test
    JsonServer::getInstance().stop();
  }

  void TearDown() override {
    // Ensure server is stopped after each test
    JsonServer::getInstance().stop();
  }
};

TEST_F(JsonServerTest, EchoSingleLine) {
  std::istringstream input("hello world");
  std::ostringstream output;

  // Start server in a separate thread
  std::thread serverThread(
      [&]() { JsonServer::getInstance().start(input, output); });

  // Give the server a moment to process
  std::this_thread::sleep_for(std::chrono::milliseconds(10));

  // Stop the server
  JsonServer::getInstance().stop();

  // Wait for server thread to finish
  if (serverThread.joinable()) {
    serverThread.join();
  }

  std::string result = output.str();
  EXPECT_TRUE(result.find("Echo Server started") != std::string::npos);
  EXPECT_TRUE(result.find("hello world") != std::string::npos);
}
