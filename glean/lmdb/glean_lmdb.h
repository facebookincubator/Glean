/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include "glean/storage/db.h"

namespace facebook {
namespace glean {
namespace lmdb {

using namespace facebook::glean::db;

using rts::Id;
using rts::Pid;

std::unique_ptr<Container> open(const std::string& path, Mode mode);

void restore(const std::string& target, const std::string& source);

} // namespace lmdb
} // namespace glean
} // namespace facebook
