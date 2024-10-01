/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include <glean/cpp/sender.h>

namespace facebook {
namespace glean {

// A Sender which dumps all data into a file. This happens on the final flush,
// rebaseAndSend is a noop. This is mostly useful for testing.
std::unique_ptr<Sender> fileWriter(std::string path);

} // namespace glean
} // namespace facebook
