/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include <optional>
#include <string>

namespace glean {
namespace swift {

/**
 * Check if help argument is present and print help if needed
 * @param argc Argument count
 * @param argv Argument vector
 * @param clientType The type of client ("Local" or "Remote")
 * @return true if help was printed and program should exit, false otherwise
 */
bool checkAndPrintHelp(int argc, char** argv, const char* clientType);

/**
 * Check if test-run argument is present and remove it from argv
 * @param argc Reference to argument count (will be modified)
 * @param argv Argument vector (will be modified)
 * @return true if --test-run argument was found, false otherwise
 */
bool checkAndRemoveTestRun(int& argc, char** argv);

std::optional<std::string> getHgRoot();

} // namespace swift
} // namespace glean
