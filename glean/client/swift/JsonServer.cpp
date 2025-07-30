/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "glean/client/swift/JsonServer.h"
#include <iostream>

JsonServer::JsonServer() : running_(false) {}

JsonServer& JsonServer::getInstance() {
  static JsonServer instance;
  return instance;
}

void JsonServer::start() {
  running_ = true;
  std::cout << "Echo Server started. Type messages and press Enter."
            << std::endl;

  std::string line;
  while (running_ && std::getline(std::cin, line)) {
    if (!line.empty()) {
      std::cout << line << std::endl;
    }
  }
}

void JsonServer::stop() {
  running_ = false;
  std::cout << "\nServer stopping..." << std::endl;
}
