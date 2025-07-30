/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include "glean/client/swift/IGlassAccess.h"

class GlassAccessMock : public IGlassAccess {
 public:
  GlassAccessMock() = default;
  ~GlassAccessMock() override = default;

  std::optional<protocol::LocationList> usrToDefinition(
      const std::string& usr) override {
    // Mock implementation - always returns nullopt
    return std::nullopt;
  }
};
