/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "glean/rts/fact.h"
#include "glean/rts/binary.h"

#include <gtest/gtest.h>
#include <cstdint>
#include <stdexcept>

namespace facebook {
namespace glean {
namespace rts {

namespace {

// Build a serialized fact "by hand" matching the wire layout that
// Fact::serialize produces: packed type, packed key_size, packed value_size,
// then `payload` (the raw key+value bytes). This lets a test supply a header
// whose sizes disagree with the payload actually present.
binary::Output clauseHeader(
    Pid type,
    uint32_t key_size,
    uint32_t value_size,
    folly::ByteRange payload) {
  binary::Output output;
  output.packed(type);
  output.packed(key_size);
  output.packed(value_size);
  output.put(payload);
  return output;
}

} // namespace

// Negative control: a well-formed fact round-trips through
// serialize/deserialize unchanged. key_size + value_size == 8 does not
// overflow, so the fix (widening the sum to size_t) leaves this path
// byte-for-byte identical -- it passes on both the vulnerable and fixed code.
// @remedimate-generated T273453249
TEST(FactTest, RoundTripsWellFormedClause) {
  unsigned char data[] = "keyvalue"; // 3-byte key "key", 5-byte value "value"
  auto original = Fact::Clause::from(folly::ByteRange(data, 8), 3);

  binary::Output output;
  Fact::serialize(output, Pid::lowest(), original);

  binary::Input input(output.bytes());
  Pid type;
  Fact::Clause clause;
  Fact::deserialize(input, type, clause);

  EXPECT_EQ(type, Pid::lowest());
  EXPECT_EQ(clause.key_size, 3u);
  EXPECT_EQ(clause.value_size, 5u);
  EXPECT_EQ(clause.key(), original.key());
  EXPECT_EQ(clause.value(), original.value());
  EXPECT_TRUE(input.empty());
}

// Positive control: a clause header whose key_size + value_size overflows
// uint32_t must be rejected, not turned into a Clause that describes far more
// bytes than the input holds. 0xFFFFFFF0 + 0x20 wraps to 0x10 in 32-bit
// arithmetic, so on the vulnerable code the bounds check was want(0x10) --
// satisfied by the 32-byte payload below -- and deserialize returned a Clause
// claiming ~4 GB backed by 32 bytes (the OOB-read source). The fix computes
// static_cast<size_t>(key_size) + value_size, so want() sees the true ~4 GB
// total, exceeds the buffer, and throws. Assert the input is rejected.
// @remedimate-generated T273453249
TEST(FactTest, RejectsKeyValueSizeOverflow) {
  unsigned char payload[32] = {0};
  auto output = clauseHeader(
      Pid::lowest(),
      0xFFFFFFF0u,
      0x20u,
      folly::ByteRange(payload, sizeof(payload)));

  binary::Input input(output.bytes());
  Pid type;
  Fact::Clause clause;
  EXPECT_THROW(Fact::deserialize(input, type, clause), std::runtime_error);
}

} // namespace rts
} // namespace glean
} // namespace facebook
