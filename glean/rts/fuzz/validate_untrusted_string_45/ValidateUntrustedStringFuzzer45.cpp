/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "glean/rts/binary.h"
#include "glean/rts/string.h"
#include "security/lionhead/utils/lib_ftest/ftest.h"

using namespace facebook::security::lionhead::fdp;

FUZZ(ValidateUntrustedStringFuzzer45, BasicFuzz) {
  auto data = f.bytes_max_length("input", 4096);
  if (data.empty()) {
    return;
  }
  folly::ByteRange range(data.data(), data.size());
  try {
    facebook::glean::rts::validateUntrustedString(range);
  } catch (...) {
  }
  try {
    facebook::glean::binary::Output output;
    facebook::glean::rts::demangleUntrustedString(range, output);
  } catch (...) {
  }
}
