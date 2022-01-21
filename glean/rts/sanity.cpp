/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "glean/rts/sanity.h"
#include <gflags/gflags.h>

DEFINE_bool(sanity_checks, false, "perform sanity checkes (slow)");

namespace facebook {
namespace glean {
namespace rts {

bool Sanity::enabled = FLAGS_sanity_checks;

}
}
}
