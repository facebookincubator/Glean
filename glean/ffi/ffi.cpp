/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "glean/ffi/ffi.h"
#include "glean/ffi/wrap.h"

namespace facebook {
namespace glean {
namespace ffi {

const char *outOfMemory = "out of memory";
const char *unknownError = "unknown error";

}
}
}

extern "C" {

void glean_ffi_free_error(const char *err) {
  if (err != facebook::glean::ffi::outOfMemory
      && err != facebook::glean::ffi::unknownError) {
    std::free(const_cast<char *>(err));
  }
}

}
