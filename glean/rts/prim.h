/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */


#pragma once

#include "glean/rts/binary.h"

#include <folly/Range.h>

namespace facebook {
namespace glean {
namespace rts {

/// Converting relative byte spans to absolute bytespans
void relToAbsByteSpans(folly::ByteRange, binary::Output&);

/// Converting packed byte spans to absolute bytespans
void unpackByteSpans(folly::ByteRange range, binary::Output& output);

}
}
}
