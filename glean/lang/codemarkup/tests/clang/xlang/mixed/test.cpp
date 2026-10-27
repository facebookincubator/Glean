/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "sidl_types.h"
#include "test_types.h"

int useBoth(BindParams params, GetNavigationRequest request) {
  EchoEnum e = EchoEnum::EchoFirst;
  CountOnlyMode mode = CountOnlyMode::Disabled;
  return params.version + request.get_identifier() + static_cast<int>(e) +
      static_cast<int>(mode);
}
