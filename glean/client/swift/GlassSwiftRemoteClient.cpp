/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <glog/logging.h>
#include <csignal>
#include <memory>
#include "folly/Singleton.h"
#include "folly/init/Init.h"
#include "glean/client/swift/GlassAccessRemote.h"
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
  if (glean::swift::checkAndPrintHelp(argc, argv, "Remote")) {
    return 0;
  }

  // Check if test-run mode is enabled and remove the argument
  bool testRun = glean::swift::checkAndRemoveTestRun(argc, argv);

  folly::Init init(&argc, &argv);

  // Configure glog to output to stderr only in test mode
  if (testRun) {
    FLAGS_logtostderr = true;
    FLAGS_minloglevel = 0; // INFO level and above
  }

  std::signal(SIGINT, signalHandler);

  // Create GlassAccessRemote and set it in JsonServer
  auto glassAccess = std::make_unique<GlassAccessRemote>();
  jsonServerSingleton.try_get()->setGlassAccess(std::move(glassAccess));

  // Start the server
  jsonServerSingleton.try_get()->start();

  return 0;
}
