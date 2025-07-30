/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <csignal>
#include <memory>
#include "folly/Singleton.h"
#include "folly/init/Init.h"
#include "glean/client/swift/GlassAccessRemote.h"
#include "glean/client/swift/JsonServer.h"

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
  folly::Init init(&argc, &argv);

  std::signal(SIGINT, signalHandler);

  // Create GlassAccessRemote and set it in JsonServer
  auto glassAccess = std::make_unique<GlassAccessRemote>();
  jsonServerSingleton.try_get()->setGlassAccess(std::move(glassAccess));

  // Start the server
  jsonServerSingleton.try_get()->start();

  return 0;
}
