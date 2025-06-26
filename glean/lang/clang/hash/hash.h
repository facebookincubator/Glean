/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include <folly/Range.h>
#include <string>

namespace facebook::glean::clangx::hash {

std::string hash(folly::ByteRange input);

} // namespace facebook::glean::clangx::hash

extern "C" {

const char *hash_ffi(const char *input, // nul-terminated
                     char *output, size_t output_size, int *result);
}
