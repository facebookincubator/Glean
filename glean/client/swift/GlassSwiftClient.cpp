/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <csignal>
#include "folly/init/Init.h"
#include "glean/client/swift/JsonServer.h"

void signalHandler(int signal) {
  if (signal == SIGINT) {
    JsonServer::getInstance().stop();
  }
}

int main(int argc, char** argv) {
  folly::Init init(&argc, &argv);

  std::signal(SIGINT, signalHandler);

  JsonServer::getInstance().start();

  return 0;
}
