// Copyright (c) Facebook, Inc. and its affiliates.

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
