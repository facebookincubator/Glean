/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "glean/client/swift/Utils.h"
#include <iostream>
#include <string>

namespace glean {
namespace swift {

void printHelp(const char* programName, const char* clientType) {
  std::cout << "Usage: " << programName << " [options]\n\n";
  std::cout << "Glass Swift " << clientType << " Client - A " << clientType
            << " JSON server for Swift language support\n\n";
  std::cout << "Options:\n";
  std::cout << "  --help, -h    Show this help message and exit\n";
  std::cout << "\nThis program starts a " << clientType
            << " JSON server that provides Swift language\n";
  std::cout << "support through the Glass Access " << clientType
            << " interface.\n";
  std::cout << "\nPress Ctrl+C to stop the server.\n";
}

bool checkAndPrintHelp(int argc, char** argv, const char* clientType) {
  // Check for help argument before folly::Init
  for (int i = 1; i < argc; ++i) {
    if (std::string(argv[i]) == "--help" || std::string(argv[i]) == "-h") {
      printHelp(argv[0], clientType);
      return true;
    }
  }
  return false;
}

} // namespace swift
} // namespace glean
