/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "glean/rts/fact.h"
#include "glean/rts/binary.h"
#include "glean/rts/inventory.h"
#include "glean/rts/serialize.h"

#include <gtest/gtest.h>
#include <cstdint>
#include <limits>
#include <stdexcept>
#include <string>
#include <vector>

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

namespace {

// A serialized Inventory, in the format Inventory::serialize writes, whose
// predicates have the given ids and empty bytecode.
std::string serializedInventory(const std::vector<uint64_t>& pids) {
  const Subroutine empty({}, 0, 0, 0, {}, {});
  binary::Output out;
  serialize::put(out, uint64_t{pids.size()});
  for (auto pid : pids) {
    serialize::put(out, Pid::fromWord(pid));
    serialize::put(out, std::string("p"));
    serialize::put(out, int32_t{1});
    serialize::put(out, empty);
    serialize::put(out, empty);
  }
  return out.string();
}

// Predicates with the given ids, in the given order, and no bytecode.
std::vector<Predicate> predicatesWithPids(const std::vector<uint64_t>& pids) {
  std::vector<Predicate> predicates;
  predicates.reserve(pids.size());
  for (auto pid : pids) {
    predicates.push_back(
        Predicate{Pid::fromWord(pid), "p", 1, nullptr, nullptr});
  }
  return predicates;
}

} // namespace

TEST(InventoryTest, RoundTripsDensePredicateIds) {
  const auto bytes =
      serializedInventory({Pid::lowest().toWord(), Pid::lowest().toWord() + 1});
  EXPECT_EQ(
      Inventory::deserialize(binary::byteRange(bytes)).serialize(), bytes);
}

// For the largest predicate id, `id + 1` wraps to 0, so the predicate table is
// sized from the other ids and the largest one is stored before its start. The
// other ids are valid, so only the largest one can be rejected.
TEST(InventoryTest, RejectsPredicateIdAtTheTopOfTheIdRange) {
  const auto bytes = serializedInventory(
      {Pid::lowest().toWord(),
       2000,
       std::numeric_limits<Pid::word_type>::max()});
  EXPECT_THROW(
      Inventory::deserialize(binary::byteRange(bytes)), std::runtime_error);
}

// An id of 0 after a nonzero id lowers the smallest id to 0, so the predicate
// table is sized from `Pid::lowest()` but indexed from 0.
TEST(InventoryTest, RejectsPredicateIdZero) {
  EXPECT_THROW(Inventory{predicatesWithPids({1100, 0})}, std::runtime_error);
}

// An id of 0 before the other ids leaves the smallest id at 1100, and the 0
// itself is stored at an index that wraps to before the table.
TEST(InventoryTest, RejectsPredicateIdZeroBeforeOtherIds) {
  EXPECT_THROW(Inventory{predicatesWithPids({0, 1100})}, std::runtime_error);
}

// An id of 0 on its own gives an empty table, which the 0 is stored into.
TEST(InventoryTest, RejectsPredicateIdZeroAlone) {
  EXPECT_THROW(Inventory{predicatesWithPids({0})}, std::runtime_error);
}

TEST(InventoryTest, AcceptsPredicateIdJustBelowTheTopOfTheIdRange) {
  const auto bytes =
      serializedInventory({std::numeric_limits<Pid::word_type>::max() - 1});
  EXPECT_EQ(
      Inventory::deserialize(binary::byteRange(bytes)).serialize(), bytes);
}

} // namespace rts
} // namespace glean
} // namespace facebook
