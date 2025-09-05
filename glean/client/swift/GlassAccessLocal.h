/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include "glean/client/swift/GlassAccess.h"

class GlassAccessLocal : public GlassAccess {
 public:
  explicit GlassAccessLocal(const std::string& root);
  ~GlassAccessLocal() override = default;
};
