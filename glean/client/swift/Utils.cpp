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
  std::cout << "  --test-run    Enable test mode with stderr logging\n";
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

bool checkAndRemoveTestRun(int& argc, char** argv) {
  // Check for test-run argument and remove it if found
  bool found = false;
  for (int i = 1; i < argc; ++i) {
    if (std::string(argv[i]) == "--test-run") {
      found = true;
      // Shift remaining arguments left to remove --test-run
      for (int j = i; j < argc - 1; ++j) {
        argv[j] = argv[j + 1];
      }
      argc--; // Decrease argument count
      i--; // Check the same position again since we shifted
    }
  }
  return found;
}

std::optional<std::string> getHgRoot() {
  // Execute 'hg root' command to get the mercurial repository root
  FILE* pipe = popen("hg root", "r");
  if (!pipe) {
    return std::nullopt;
  }

  char buffer[1024];
  std::string result;
  while (fgets(buffer, sizeof(buffer), pipe) != nullptr) {
    result += buffer;
  }

  int status = pclose(pipe);
  if (status != 0) {
    return std::nullopt;
  }

  // Remove trailing newline if present
  if (!result.empty() && result.back() == '\n') {
    result.pop_back();
  }

  if (result.empty()) {
    return std::nullopt;
  }

  return result;
}

} // namespace swift
} // namespace glean
