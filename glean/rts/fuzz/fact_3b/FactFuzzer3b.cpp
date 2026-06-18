/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

// =============================================================
// Harness Intention:
//   Fuzzes Fact::deserialize to trigger integer overflow when
//   key_size + value_size overflows uint32_t, causing truncated
//   read and heap-buffer-overflow.
//
// Improvements:
//   - Exercise Clause accessors (key, value, bytes, size) after
//     successful deserialization to cover more code in fact.h
//   - Added round-trip test (serialize then deserialize) to
//     exercise Fact::serialize and improve coverage of fact.cpp
//   - Exercise Fact::create and Fact::dump to cover fact object
//     construction and string representation
//   - Guard Fact::create against overflow to prevent secondary
//     crashes while preserving the primary vulnerability trigger
// =============================================================

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

    // Exercise clause accessors to cover more code paths in fact.h
    // Note: clause.size() uses uint32_t addition which may overflow,
    // but just computing the value is safe (no memory access).
    auto k = clause.key();
    auto s = clause.size();
    (void)k;
    (void)s;

    // Round-trip: serialize then deserialize again to exercise
    // Fact::serialize (fact.cpp:18-23)
    Output output;
    Fact::serialize(output, type, clause);

    Input input2(output.bytes());
    Pid type2;
    Fact::Clause clause2;
    Fact::deserialize(input2, type2, clause2);

    // Exercise Fact::create and Fact::dump to cover fact object
    // construction (fact.h) and string representation (fact.cpp:33-51)
    // Guard against integer overflow in key_size + value_size to prevent
    // secondary crashes in Fact::create/dump. The primary vulnerability
    // (integer overflow in Fact::deserialize) is still triggered above.
    auto total = static_cast<uint64_t>(clause2.key_size) + clause2.value_size;
    if (clause2.key_size <= Fact::MAX_KEY_SIZE && total <= UINT32_MAX) {
      Fact::Ref ref;
      ref.id = Id::fromWord(1);
      ref.type = type2;
      ref.clause = clause2;
      auto fact = Fact::create(ref);
      if (fact) {
        auto dump_str = fact->dump();
        (void)dump_str;
      }
    }
  } catch (...) {
  }
}
