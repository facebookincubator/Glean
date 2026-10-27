/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

// Forward-declares the generated struct instead of including test_types.h.
namespace glean::sidl {
struct Options;
} // namespace glean::sidl

int optionsRetries(const glean::sidl::Options& options);
