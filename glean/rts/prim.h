// Copyright (c) Facebook, Inc. and its affiliates.


#pragma once

#include "glean/rts/binary.h"

#include <folly/Range.h>

namespace facebook {
namespace glean {
namespace rts {

/// Converting relative byte spans to absolute bytespans
void relToAbsByteSpans(folly::ByteRange, binary::Output&);

}
}
}
