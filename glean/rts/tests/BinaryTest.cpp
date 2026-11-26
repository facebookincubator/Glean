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
#include <stdexcept>
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

// Each buffer is a heap allocation holding exactly the bytes under
// validation, so any read past its end is visible to AddressSanitizer.
TEST(BinaryTest, untrustedAsciiStringWithoutTerminatorIsRejected) {
  const std::vector<unsigned char> unterminated{'a', 'b', 'c'};
  EXPECT_THROW(
      validateUntrustedString(binary::byteRange(unterminated)),
      std::runtime_error);
}

TEST(BinaryTest, untrustedStringEndingInEscapedNulIsRejected) {
  const std::vector<unsigned char> unterminated{'a', 'b', 0, 1};
  binary::Input input(binary::byteRange(unterminated));
  binary::Output demangled;
  EXPECT_THROW(input.demangleUntrustedString(demangled), std::runtime_error);
}

TEST(BinaryTest, untrustedNonAsciiStringWithoutTerminatorIsRejected) {
  // U+00E9 encoded as UTF-8
  const std::vector<unsigned char> unterminated{'a', 0xc3, 0xa9};
  EXPECT_THROW(
      validateUntrustedString(binary::byteRange(unterminated)),
      std::runtime_error);
}

TEST(BinaryTest, untrustedEmptyStringAtEndOfBufferIsRejected) {
  const std::vector<unsigned char> buffer{'a'};
  const folly::ByteRange atEnd(buffer.data() + buffer.size(), size_t{0});
  EXPECT_THROW(validateUntrustedString(atEnd), std::runtime_error);
}

TEST(BinaryTest, untrustedStringValidationStopsAtEndOfRange) {
  // The byte after the range is invalid UTF-8, so an error that mentions it
  // means validation read past the end. This needs no sanitizer.
  const std::vector<unsigned char> buffer{'a', 'b', 'c', 0xff};
  const folly::ByteRange range(buffer.data(), size_t{3});
  try {
    validateUntrustedString(range);
    ADD_FAILURE() << "unterminated string was accepted";
  } catch (const std::runtime_error& e) {
    EXPECT_NE(
        std::string(e.what()).find("truncated terminator"), std::string::npos)
        << e.what();
  }
}

TEST(BinaryTest, untrustedStringEndingExactlyAtBufferEndIsAccepted) {
  const std::vector<unsigned char> mangled{'a', 0, 1, 0xc3, 0xa9, 0, 0};
  binary::Input input(binary::byteRange(mangled));
  binary::Output demangled;
  input.demangleUntrustedString(demangled);
  EXPECT_TRUE(input.empty());
  EXPECT_EQ("a\0\xc3\xa9"s, binary::mkString(demangled.bytes()));
}

TEST(BinaryTest, toLowerOfMangledStringLowersCharacters) {
  binary::Output mangled;
  // A, U+00C9, NUL, Z, U+10A0, U+023A (lowers to 3 bytes), U+10400 (4 bytes)
  mangled.mangleString(
      binary::byteRange("A\xc3\x89\0Z\xe1\x82\xa0\xc8\xba\xf0\x90\x90\x80"s));
  binary::Output expected;
  expected.mangleString(
      binary::byteRange(
          "a\xc3\xa9\0z\xe2\xb4\x80\xe2\xb1\xa5\xf0\x90\x90\xa8"s));
  binary::Output lowered;
  toLowerTrustedString(mangled.bytes(), lowered);
  EXPECT_EQ(
      binary::mkString(expected.bytes()), binary::mkString(lowered.bytes()));
}

TEST(BinaryTest, toLowerOfStringWithTruncatedUtf8IsRejected) {
  // Each text ends in a lead byte whose continuation bytes are missing, so
  // decoding it would run into the terminator or past the end of the buffer.
  const std::vector<std::vector<unsigned char>> inputs{
      {'x', 0xf0, 0, 0},
      {'x', 0xe0, 0, 0},
      {0xe0, 0, 0},
      {0xc3, 0, 0},
  };
  for (const auto& mangled : inputs) {
    binary::Output lowered;
    EXPECT_THROW(
        toLowerTrustedString(binary::byteRange(mangled), lowered),
        std::runtime_error);
  }
}

TEST(BinaryTest, toLowerOfIllFormedUtf8MidStringIsRejected) {
  // An encoded surrogate and an overlong NUL, each followed by more text
  const std::vector<std::vector<unsigned char>> inputs{
      {'x', 0xed, 0xa0, 0x80, 'y', 0, 0},
      {0xc0, 0x80, 'y', 0, 0},
  };
  for (const auto& mangled : inputs) {
    binary::Output lowered;
    EXPECT_THROW(
        toLowerTrustedString(binary::byteRange(mangled), lowered),
        std::runtime_error);
  }
}

TEST(BinaryTest, toLowerStopsAtTheEndOfAnUnterminatedRange) {
  // No terminator, so only the range bound stops the decode: a sequence cut
  // off by the end of the range is rejected, and one that ends exactly there
  // is decoded.
  const std::vector<unsigned char> truncated{'x', 0xe1, 0x80};
  binary::Output rejected;
  EXPECT_THROW(
      toLowerTrustedString(binary::byteRange(truncated), rejected),
      std::runtime_error);

  // U+10A0, which lowers to U+2D00
  const std::vector<unsigned char> complete{'X', 0xe1, 0x82, 0xa0};
  binary::Output lowered;
  toLowerTrustedString(binary::byteRange(complete), lowered);
  EXPECT_EQ("x\xe2\xb4\x80"s, binary::mkString(lowered.bytes()));
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
