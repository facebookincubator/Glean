/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "test.h"

// Sees only the forward declaration in test.h, never the annotated definition.
int retriesOf(const glean::sidl::Options& options) {
  return optionsRetries(options);
}
