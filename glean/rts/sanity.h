// Copyright (c) Facebook, Inc. and its affiliates.

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
