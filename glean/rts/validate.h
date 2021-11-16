/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include "glean/rts/inventory.h"
#include "glean/rts/lookup.h"

#include <limits>

namespace facebook {
namespace glean {
namespace rts {

struct Validate {
  bool typecheck = true;
  bool keys = true;
  size_t limit = std::numeric_limits<size_t>::max();
};

void validate(const Inventory& inventory, const Validate& val, Lookup& facts);

}
}
}
