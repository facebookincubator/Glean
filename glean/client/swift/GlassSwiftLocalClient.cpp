/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <csignal>
#include <memory>
#include <utility>
#include "folly/Singleton.h"
#include "folly/init/Init.h"
#include "glean/client/swift/GlassAccessLocal.h"
#include "glean/client/swift/JsonServer.h"
#include "glean/client/swift/Utils.h"

// Folly singleton for JsonServer
folly::Singleton<JsonServer> jsonServerSingleton([]() {
  return new JsonServer();
});

void signalHandler(int signal) {
  if (signal == SIGINT) {
    jsonServerSingleton.try_get()->stop();
  }
}

int main(int argc, char** argv) {
  if (glean::swift::checkAndPrintHelp(argc, argv, "Local")) {
    return 0;
  }

  folly::Init init(&argc, &argv);

  std::signal(SIGINT, signalHandler);

  // Create GlassAccessLocal and set it in JsonServer
  auto glassAccess = std::make_unique<GlassAccessLocal>();
  jsonServerSingleton.try_get()->setGlassAccess(std::move(glassAccess));

  // Start the server
  jsonServerSingleton.try_get()->start();

  return 0;
}
