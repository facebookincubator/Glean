/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "glean/rts/binary.h"
#include "glean/rts/set.h"

#include <glog/logging.h>
#include <gtest/gtest.h>
#include <cstdint>
#include <string>
#include <vector>

namespace facebook {
namespace glean {
namespace rts {

namespace {

void roundtripString(const std::string& s) {
  binary::Output original;
  original.mangleString(binary::byteRange(s));
  binary::Input inp(original.bytes());
  binary::Output demangled;
  inp.demangleUntrustedString(demangled);
  CHECK_EQ(s, binary::mkString(demangled.bytes()));
}

binary::Output packedValue(uint64_t value) {
  binary::Output output;
  output.packed(value);
  return output;
}

std::vector<uint64_t> decodePackedArray(const binary::Output& output) {
  binary::Input input(output.bytes());
  const auto size = input.packed<uint64_t>();
  std::vector<uint64_t> values;
  values.reserve(size);
  for (uint64_t i = 0; i < size; ++i) {
    values.push_back(input.packed<uint64_t>());
  }
  EXPECT_TRUE(input.empty());
  return values;
}

std::vector<uint8_t> decodeByteArray(const binary::Output& output) {
  binary::Input input(output.bytes());
  const auto size = input.packed<uint64_t>();
  std::vector<uint8_t> values;
  values.reserve(size);
  for (uint64_t i = 0; i < size; ++i) {
    values.push_back(input.fixed<uint8_t>());
  }
  EXPECT_TRUE(input.empty());
  return values;
}

} // namespace

using namespace std::string_literals;

TEST(BinaryTest, utf8String) {
  roundtripString(""s);
  roundtripString("abcd"s);
  roundtripString("\0"s);
  roundtripString("abc\0def"s);
  roundtripString("abc\0\0");
}

TEST(SetOpsTest, OutputSetDeduplicatesAndUsesSerializedByteOrder) {
  SetOps sets;
  const auto token = sets.newSet();
  auto highByteFirst = packedValue(255);
  auto lowByteFirst = packedValue(256);
  auto duplicate = packedValue(255);

  sets.insertOutputSet(token, &highByteFirst);
  sets.insertOutputSet(token, &lowByteFirst);
  sets.insertOutputSet(token, &duplicate);
  binary::Output encoded;
  sets.setToArray(token, &encoded);

  const std::vector<uint64_t> expected{256, 255};
  EXPECT_EQ(decodePackedArray(encoded), expected);
}

TEST(SetOpsTest, OutputSetRejectsInsertionPastByteLimitWithoutMutation) {
  SetOps sets(2);
  const auto token = sets.newSet();
  auto accepted = packedValue(128);
  sets.insertOutputSet(token, &accepted);
  auto rejected = packedValue(1);

  EXPECT_THROW(sets.insertOutputSet(token, &rejected), std::runtime_error);
  binary::Output encoded;
  sets.setToArray(token, &encoded);

  const std::vector<uint64_t> expected{128};
  EXPECT_EQ(decodePackedArray(encoded), expected);
}

TEST(SetOpsTest, FreeSetCompactsTokensAndTheirSizeAccountingTogether) {
  SetOps sets(1);
  const auto discardedToken = sets.newSet();
  const auto retainedToken = sets.newSet();
  auto discarded = packedValue(1);
  auto retained = packedValue(2);
  sets.insertOutputSet(discardedToken, &discarded);
  sets.insertOutputSet(retainedToken, &retained);

  sets.freeSet(discardedToken);
  auto overLimit = packedValue(3);
  EXPECT_THROW(sets.insertOutputSet(0, &overLimit), std::runtime_error);
  binary::Output encoded;
  sets.setToArray(0, &encoded);

  const std::vector<uint64_t> expected{2};
  EXPECT_EQ(decodePackedArray(encoded), expected);
}

TEST(SetOpsTest, WordSetDeduplicatesAndSerializesInNumericOrder) {
  SetOps sets;
  const auto token = sets.newWordSet();

  sets.insertWordSet(token, 42);
  sets.insertWordSet(token, 42);
  sets.insertWordSet(token, 7);
  binary::Output encoded;
  sets.wordSetToArray(token, &encoded);

  const std::vector<uint64_t> expected{7, 42};
  EXPECT_EQ(decodePackedArray(encoded), expected);
}

TEST(SetOpsTest, WordSetRejectsValuePastSizeLimitWithoutMutation) {
  SetOps sets(2 * sizeof(uint64_t));
  const auto token = sets.newWordSet();
  sets.insertWordSet(token, 5);
  sets.insertWordSet(token, 10);

  EXPECT_THROW(sets.insertWordSet(token, 15), std::runtime_error);
  binary::Output encoded;
  sets.wordSetToArray(token, &encoded);

  const std::vector<uint64_t> expected{5, 10};
  EXPECT_EQ(decodePackedArray(encoded), expected);
}

TEST(SetOpsTest, ByteSetDeduplicatesAndSerializesAsSortedBytes) {
  SetOps sets;
  const auto token = sets.newWordSet();
  const unsigned char values[]{255, 3, 3, 0};

  sets.insertBytesWordSet(token, values, values + std::size(values));
  binary::Output encoded;
  sets.byteSetToByteArray(token, &encoded);

  const std::vector<uint8_t> expected{0, 3, 255};
  EXPECT_EQ(decodeByteArray(encoded), expected);
}

TEST(SetOpsTest, ByteSetRejectsOversizedBatchWithoutPartialInsertion) {
  SetOps sets(2);
  const auto token = sets.newWordSet();
  const unsigned char oversized[]{1, 2, 3};

  EXPECT_THROW(
      sets.insertBytesWordSet(
          token, oversized, oversized + std::size(oversized)),
      std::runtime_error);
  const unsigned char accepted[]{4, 5};
  sets.insertBytesWordSet(token, accepted, accepted + std::size(accepted));
  binary::Output encoded;
  sets.byteSetToByteArray(token, &encoded);

  const std::vector<uint8_t> expected{4, 5};
  EXPECT_EQ(decodeByteArray(encoded), expected);
}

TEST(SetOpsTest, FreeWordSetMakesTheFollowingSetTheFirstToken) {
  SetOps sets;
  const auto discardedToken = sets.newWordSet();
  const auto retainedToken = sets.newWordSet();
  sets.insertWordSet(discardedToken, 1);
  sets.insertWordSet(retainedToken, 2);

  sets.freeWordSet(discardedToken);
  binary::Output encoded;
  sets.wordSetToArray(0, &encoded);

  const std::vector<uint64_t> expected{2};
  EXPECT_EQ(decodePackedArray(encoded), expected);
}

} // namespace rts
} // namespace glean
} // namespace facebook
