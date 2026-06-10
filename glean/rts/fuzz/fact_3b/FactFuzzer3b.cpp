/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

// Fuzzing harness for Fact::deserialize (fbcode/glean/rts/fact.cpp:25).
// Targets integer overflow where key_size + value_size overflows uint32_t,
// causing a truncated read and subsequent heap-buffer-overflow.

#include "glean/rts/binary.h"
#include "glean/rts/fact.h"

#include "security/lionhead/utils/lib_ftest/ftest.h"

using namespace facebook::glean::binary;
using namespace facebook::glean::rts;

FUZZ(FactFuzzer3b, FuzzDeserialize) {
  auto data = f.all_remaining_bytes();
  Input input(folly::ByteRange(data.data(), data.size()));

  Pid type;
  Fact::Clause clause;
  try {
    Fact::deserialize(input, type, clause);
  } catch (...) {
  }
}
