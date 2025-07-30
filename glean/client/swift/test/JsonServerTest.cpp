/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "glean/client/swift/JsonServer.h"
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <chrono>
#include <memory>
#include <sstream>
#include <thread>
#include "glean/client/swift/test/GlassAccessMock.h"

using ::testing::Return;

class JsonServerTest : public ::testing::Test {
 protected:
  void SetUp() override {
    // Create JsonServer and set mock GlassAccess
    server_ = std::make_unique<JsonServer>();
    mockGlassAccess_ = std::make_unique<GlassAccessMock>();

    // Set up default expectations for the mock
    protocol::LocationList defaultLocations;
    protocol::Position start(25, 4);
    protocol::Position end(25, 30);
    protocol::Range range(start, end);
    protocol::Location location("file:///path/to/definition/file", range);
    defaultLocations.push_back(location);

    ON_CALL(
        *mockGlassAccess_,
        usrToDefinition(
            "s:12IGFriendsMap0aB4ViewC03mapC0So05MKMapC0CvgAFyXEfU_ADL_AFvp"))
        .WillByDefault(Return(defaultLocations));

    ON_CALL(*mockGlassAccess_, usrToDefinition("unknown_usr"))
        .WillByDefault(Return(std::nullopt));

    server_->setGlassAccess(std::move(mockGlassAccess_));
  }

  void TearDown() override {
    // Ensure server is stopped after each test
    if (server_) {
      server_->stop();
    }
  }

  std::unique_ptr<JsonServer> server_;
  std::unique_ptr<GlassAccessMock> mockGlassAccess_;
};

TEST_F(JsonServerTest, USRToDefinitionRequest) {
  // Test the LSP-like protocol with USRToDefinition request
  std::string request =
      R"({"id": 1, "revision": "17186b417211", "method": "USRToDefinition", "value": "s:12IGFriendsMap0aB4ViewC03mapC0So05MKMapC0CvgAFyXEfU_ADL_AFvp"})";

  std::ostringstream output;

  // Process the request directly without threading
  server_->processRequest(request, output);

  std::string result = output.str();

  // Verify the response contains expected JSON structure
  EXPECT_TRUE(result.find("\"id\":1") != std::string::npos);
  EXPECT_TRUE(result.find("\"result\"") != std::string::npos);
  EXPECT_TRUE(
      result.find("\"uri\":\"file:///path/to/definition/file\"") !=
      std::string::npos);
  EXPECT_TRUE(result.find("\"line\":25") != std::string::npos);
  EXPECT_TRUE(result.find("\"character\":4") != std::string::npos);
  EXPECT_TRUE(result.find("\"character\":30") != std::string::npos);
}

TEST_F(JsonServerTest, USRToDefinitionRequestNoResults) {
  // Test USRToDefinition request with unknown USR (should return empty result)
  std::string request =
      R"({"id": 2, "method": "USRToDefinition", "value": "unknown_usr"})";

  std::ostringstream output;

  // Process the request directly without threading
  server_->processRequest(request, output);

  std::string result = output.str();

  // Verify the response contains expected JSON structure with empty result
  EXPECT_TRUE(result.find("\"id\":2") != std::string::npos);
  EXPECT_TRUE(result.find("\"result\":[]") != std::string::npos);
}

TEST_F(JsonServerTest, UnknownMethodRequest) {
  // Test request with unknown method (should return error)
  std::string request =
      R"({"id": 3, "method": "UnknownMethod", "value": "test"})";

  std::ostringstream output;

  // Process the request directly without threading
  server_->processRequest(request, output);

  std::string result = output.str();

  // Verify the response contains error for unknown method
  EXPECT_TRUE(result.find("\"id\":3") != std::string::npos);
  EXPECT_TRUE(result.find("\"error\"") != std::string::npos);
  EXPECT_TRUE(result.find("\"code\":-32601") != std::string::npos);
  EXPECT_TRUE(
      result.find("\"message\":\"Method not found\"") != std::string::npos);
}

TEST_F(JsonServerTest, InvalidJSONRequest) {
  // Test request with invalid JSON (should return parse error)
  std::string request = "invalid json {";

  std::ostringstream output;

  // Process the request directly without threading
  server_->processRequest(request, output);

  std::string result = output.str();

  // Verify the response contains parse error
  EXPECT_TRUE(result.find("\"error\"") != std::string::npos);
  EXPECT_TRUE(result.find("\"code\":-32700") != std::string::npos);
  EXPECT_TRUE(result.find("\"message\":\"Parse error:") != std::string::npos);
}
