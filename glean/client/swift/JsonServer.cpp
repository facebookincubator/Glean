/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "glean/client/swift/JsonServer.h"

JsonServer::JsonServer() : running_(false), glassAccess_(nullptr) {}

void JsonServer::setGlassAccess(std::unique_ptr<IGlassAccess> glassAccess) {
  glassAccess_ = std::move(glassAccess);
}

void JsonServer::start(std::istream& input, std::ostream& output) {
  running_ = true;
  output << "Echo Server started. Type messages and press Enter." << std::endl;

  std::string line;
  while (running_ && std::getline(input, line)) {
    if (!line.empty()) {
      output << line << std::endl;
    }
  }
}

void JsonServer::stop() {
  running_ = false;
}
