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

// A Sender which sends data synchronously via Thrift.
std::unique_ptr<Sender> thriftSender(
    const std::string& service,
    const std::string& repo_name,
    const std::string& repo_hash,
    double min_retry_delay,
    size_t max_errors);

} // namespace glean
} // namespace facebook
