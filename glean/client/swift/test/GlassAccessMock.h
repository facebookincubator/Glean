/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include <gmock/gmock.h>
#include "glean/client/swift/IGlassAccess.h"

class GlassAccessMock : public IGlassAccess {
 public:
  GlassAccessMock() = default;
  ~GlassAccessMock() override = default;

  MOCK_METHOD(
      std::optional<protocol::LocationList>,
      usrToDefinition,
      (const std::string& usr),
      (override));
};
