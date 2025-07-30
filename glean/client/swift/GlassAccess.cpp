/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "glean/client/swift/GlassAccess.h"

std::optional<protocol::LocationList> GlassAccess::usrToDefinition(
    const std::string& usr) {
  // Default implementation - always returns nullopt
  return std::nullopt;
}
