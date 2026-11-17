/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "glean/rts/ownership/slice.h"
#include "glean/rts/serialize.h"

#include <boost/dynamic_bitset.hpp>
#include <gtest/gtest.h>
#include <cstdint>
#include <vector>

namespace facebook {
namespace glean {
namespace rts {

namespace {

std::unique_ptr<Slice> roundTrip(const Slice& slice) {
  binary::Output output;
  slice.serialize(output);
  binary::Input input(output.data(), output.size());
  return Slice::deserialize(input);
}

} // namespace

TEST(SliceTest, SerializationPreservesExactRange) {
  boost::dynamic_bitset<uint64_t> bits(70);
  bits.set(0);
  bits.set(65);
  const auto restored = roundTrip(Slice(200, bits));

  EXPECT_EQ(restored->first(), 200);
  EXPECT_EQ(restored->end(), 270);
  EXPECT_TRUE(restored->visible(200));
  EXPECT_TRUE(restored->visible(265));
  EXPECT_FALSE(restored->inRange(270));
}

TEST(SliceTest, SerializationPreservesEmptySlice) {
  const auto restored = roundTrip(Slice(42, boost::dynamic_bitset<uint64_t>()));
  EXPECT_EQ(restored->first(), 42);
  EXPECT_TRUE(restored->empty());
}

TEST(SliceTest, DeserializationAcceptsFormatWithoutSize) {
  // Slices stored before the bit count was serialized consist of `first`
  // and the words only. They must still load, rounded up to whole words.
  boost::dynamic_bitset<uint64_t> bits(70);
  bits.set(3);
  std::vector<uint64_t> words(bits.num_blocks());
  to_block_range(bits, words.begin());

  binary::Output output;
  serialize::put(output, UsetId(200));
  serialize::put(output, words, serialize::AsBytes());
  binary::Input input(output.data(), output.size());
  const auto restored = Slice::deserialize(input);

  EXPECT_EQ(restored->first(), 200);
  EXPECT_EQ(restored->end(), 200 + 128);
  EXPECT_TRUE(restored->visible(203));
}

// Regression test for https://github.com/facebookincubator/Glean/issues/738:
// a reloaded base-DB slice must not cover the UsetIds of the DB stacked on
// it, or Slices::visible() (AND over covering slices) hides the upper DB's
// facts.
TEST(SliceTest, StackedSlicesStayDisjointAfterSerialization) {
  boost::dynamic_bitset<uint64_t> baseBits(14);
  baseBits.set();
  boost::dynamic_bitset<uint64_t> layerBits(8);
  layerBits.set();
  const auto base = roundTrip(Slice(5, baseBits));
  const auto layer = roundTrip(Slice(19, layerBits));

  Slices slices({base.get(), layer.get()});
  for (UsetId id = 5; id < 27; ++id) {
    EXPECT_TRUE(slices.visible(id)) << "UsetId " << id;
  }
  EXPECT_FALSE(slices.visible(27));
}

} // namespace rts
} // namespace glean
} // namespace facebook
