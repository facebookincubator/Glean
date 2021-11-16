/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include "glog/logging.h"

namespace facebook {
namespace glean {
namespace rts {

struct Sanity {
  static bool enabled;
};

#define GLEAN_SANITY_CHECK(expr) \
  if (!::facebook::glean::rts::Sanity::enabled) {} else { \
    if (!(expr)) { \
      LOG(FATAL) << "sanity check '" #expr "'failed"; \
    } \
  }

}
}
}
