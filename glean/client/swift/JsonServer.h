/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include <atomic>
#include <iostream>

class JsonServer {
 public:
  static JsonServer& getInstance();

  void start(std::istream& input = std::cin, std::ostream& output = std::cout);
  void stop();

 private:
  JsonServer();
  ~JsonServer() = default;
  JsonServer(const JsonServer&) = delete;
  JsonServer& operator=(const JsonServer&) = delete;
  JsonServer(JsonServer&&) = delete;
  JsonServer& operator=(JsonServer&&) = delete;
  std::atomic<bool> running_;
};
