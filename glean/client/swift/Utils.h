/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

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

} // namespace swift
} // namespace glean
