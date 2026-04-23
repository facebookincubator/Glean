/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "glean/rts/factset.h"
#include <gtest/gtest.h>
#include <string>
#include <vector>
#include "glean/rts/binary.h"

using namespace facebook::glean::rts;
using namespace facebook::glean;

namespace {

Fact::Clause clauseFrom(const unsigned char* d, size_t total, size_t ks) {
  return Fact::Clause::from(folly::ByteRange(d, total), ks);
}

} // namespace

TEST(FactSetTest, DefineAddsFactAndAdvancesId) {
  FactSet fs(Id::lowest());
  unsigned char data[] = "keyval";
  auto id = fs.define(Pid::lowest(), clauseFrom(data, 6, 3));
  EXPECT_EQ(id, Id::lowest());
  EXPECT_EQ(fs.size(), 1);
  EXPECT_FALSE(fs.empty());
  EXPECT_EQ(fs.firstFreeId(), Id::lowest() + 1);
}

TEST(FactSetTest, DefineDuplicateReturnsSameId) {
  FactSet fs(Id::lowest());
  unsigned char data[] = "keyval";
  auto id1 = fs.define(Pid::lowest(), clauseFrom(data, 6, 3));
  auto id2 = fs.define(Pid::lowest(), clauseFrom(data, 6, 3));
  EXPECT_EQ(id1, id2);
  EXPECT_EQ(fs.size(), 1);
}

TEST(FactSetTest, DefineSameKeyDiffValueReturnsInvalid) {
  FactSet fs(Id::lowest());
  unsigned char d1[] = "keyAAA";
  unsigned char d2[] = "keyBBB";
  fs.define(Pid::lowest(), clauseFrom(d1, 6, 3));
  auto id2 = fs.define(Pid::lowest(), clauseFrom(d2, 6, 3));
  EXPECT_EQ(id2, Id::invalid());
}

TEST(FactSetTest, DefineDifferentKeysGetDistinctIds) {
  FactSet fs(Id::lowest());
  unsigned char d1[] = "aaa";
  unsigned char d2[] = "bbb";
  auto id1 = fs.define(Pid::lowest(), clauseFrom(d1, 3, 3));
  auto id2 = fs.define(Pid::lowest(), clauseFrom(d2, 3, 3));
  EXPECT_NE(id1, id2);
  EXPECT_EQ(fs.size(), 2);
}

TEST(FactSetTest, TypeByIdReturnsCorrectTypeOrInvalid) {
  FactSet fs(Id::lowest());
  auto pid1 = Pid::lowest();
  auto pid2 = Pid::lowest() + 1;
  unsigned char d1[] = "aa";
  unsigned char d2[] = "bb";
  auto id1 = fs.define(pid1, clauseFrom(d1, 2, 2));
  auto id2 = fs.define(pid2, clauseFrom(d2, 2, 2));
  EXPECT_EQ(fs.typeById(id1), pid1);
  EXPECT_EQ(fs.typeById(id2), pid2);
  EXPECT_EQ(fs.typeById(Id::lowest() + 99), Pid::invalid());
  EXPECT_EQ(fs.typeById(Id::invalid()), Pid::invalid());
}

TEST(FactSetTest, IdByKeyFindsExistingOrReturnsInvalid) {
  FactSet fs(Id::lowest());
  unsigned char data[] = "mykey";
  auto id = fs.define(Pid::lowest(), clauseFrom(data, 5, 5));
  EXPECT_EQ(fs.idByKey(Pid::lowest(), folly::ByteRange(data, 5)), id);
  unsigned char missing[] = "nope";
  EXPECT_EQ(
      fs.idByKey(Pid::lowest(), folly::ByteRange(missing, 4)), Id::invalid());
}

TEST(FactSetTest, FactByIdCallbackReceivesCorrectData) {
  FactSet fs(Id::lowest());
  unsigned char data[] = "keyval";
  auto id = fs.define(Pid::lowest(), clauseFrom(data, 6, 3));
  Pid gotType = Pid::invalid();
  size_t gotKeySize = 0;
  size_t gotValSize = 0;
  bool found = fs.factById(id, [&](Pid t, Fact::Clause c) {
    gotType = t;
    gotKeySize = c.key_size;
    gotValSize = c.value_size;
  });
  EXPECT_TRUE(found);
  EXPECT_EQ(gotType, Pid::lowest());
  EXPECT_EQ(gotKeySize, 3);
  EXPECT_EQ(gotValSize, 3);
}

TEST(FactSetTest, FactByIdReturnsFalseForMissing) {
  FactSet fs(Id::lowest());
  bool found = fs.factById(Id::lowest(), [](Pid, Fact::Clause) {});
  EXPECT_FALSE(found);
}

TEST(FactSetTest, CountReturnsNumberOfFactsPerPredicate) {
  FactSet fs(Id::lowest());
  auto pid1 = Pid::lowest();
  auto pid2 = Pid::lowest() + 1;
  unsigned char d1[] = "a";
  unsigned char d2[] = "b";
  unsigned char d3[] = "c";
  fs.define(pid1, clauseFrom(d1, 1, 1));
  fs.define(pid1, clauseFrom(d2, 1, 1));
  fs.define(pid2, clauseFrom(d3, 1, 1));
  EXPECT_EQ(fs.count(pid1).low(), 2);
  EXPECT_EQ(fs.count(pid2).low(), 1);
  EXPECT_EQ(fs.count(Pid::lowest() + 99).low(), 0);
}

TEST(FactSetTest, EnumerateIteratesAllFacts) {
  FactSet fs(Id::lowest());
  unsigned char d1[] = "aa";
  unsigned char d2[] = "bb";
  fs.define(Pid::lowest(), clauseFrom(d1, 2, 2));
  fs.define(Pid::lowest(), clauseFrom(d2, 2, 2));
  auto iter = fs.enumerate();
  std::vector<Id> ids;
  for (auto ref = iter->get(); ref; iter->next(), ref = iter->get()) {
    ids.push_back(ref.id);
  }
  std::vector<Id> expected{Id::lowest(), Id::lowest() + 1};
  EXPECT_EQ(ids, expected);
}

TEST(FactSetTest, EnumerateWithBounds) {
  FactSet fs(Id::lowest());
  unsigned char d1[] = "a";
  unsigned char d2[] = "b";
  unsigned char d3[] = "c";
  fs.define(Pid::lowest(), clauseFrom(d1, 1, 1));
  fs.define(Pid::lowest(), clauseFrom(d2, 1, 1));
  fs.define(Pid::lowest(), clauseFrom(d3, 1, 1));
  auto iter = fs.enumerate(Id::lowest() + 1, Id::lowest() + 2);
  auto ref = iter->get();
  ASSERT_TRUE(bool(ref));
  EXPECT_EQ(ref.id, Id::lowest() + 1);
  iter->next();
  EXPECT_FALSE(bool(iter->get()));
}

TEST(FactSetTest, EnumerateBackIteratesReverse) {
  FactSet fs(Id::lowest());
  unsigned char d1[] = "a";
  unsigned char d2[] = "b";
  unsigned char d3[] = "c";
  fs.define(Pid::lowest(), clauseFrom(d1, 1, 1));
  fs.define(Pid::lowest(), clauseFrom(d2, 1, 1));
  fs.define(Pid::lowest(), clauseFrom(d3, 1, 1));
  auto iter = fs.enumerateBack();
  std::vector<Id> ids;
  for (auto ref = iter->get(); ref; iter->next(), ref = iter->get()) {
    ids.push_back(ref.id);
  }
  std::vector<Id> expected{Id::lowest() + 2, Id::lowest() + 1, Id::lowest()};
  EXPECT_EQ(ids, expected);
}

TEST(FactSetTest, AppendCombinesTwoSets) {
  FactSet fs1(Id::lowest());
  unsigned char d1[] = "aa";
  fs1.define(Pid::lowest(), clauseFrom(d1, 2, 2));

  FactSet fs2(fs1.firstFreeId());
  unsigned char d2[] = "bb";
  fs2.define(Pid::lowest(), clauseFrom(d2, 2, 2));

  EXPECT_TRUE(fs1.appendable(fs2));
  fs1.append(std::move(fs2));
  EXPECT_EQ(fs1.size(), 2);
  EXPECT_EQ(fs1.typeById(Id::lowest()), Pid::lowest());
  EXPECT_EQ(fs1.typeById(Id::lowest() + 1), Pid::lowest());
}

TEST(FactSetTest, AppendableReturnsFalseOnIdGap) {
  FactSet fs1(Id::lowest());
  unsigned char d1[] = "aa";
  fs1.define(Pid::lowest(), clauseFrom(d1, 2, 2));

  FactSet fs2(Id::lowest() + 100);
  unsigned char d2[] = "bb";
  fs2.define(Pid::lowest(), clauseFrom(d2, 2, 2));

  EXPECT_FALSE(fs1.appendable(fs2));
}

TEST(FactSetTest, AppendableReturnsFalseOnDuplicateKey) {
  FactSet fs1(Id::lowest());
  unsigned char d1[] = "same";
  fs1.define(Pid::lowest(), clauseFrom(d1, 4, 4));

  FactSet fs2(fs1.firstFreeId());
  unsigned char d2[] = "same";
  fs2.define(Pid::lowest(), clauseFrom(d2, 4, 4));

  EXPECT_FALSE(fs1.appendable(fs2));
}

TEST(FactSetTest, AppendableWithEmptySets) {
  FactSet fs1(Id::lowest());
  FactSet fs2(Id::lowest() + 999);
  EXPECT_TRUE(fs1.appendable(fs2));
  EXPECT_TRUE(fs2.appendable(fs1));
}

TEST(FactSetTest, SerializePreservesFactData) {
  FactSet fs(Id::lowest());
  unsigned char d1[] = "key1val1";
  unsigned char d2[] = "key2val2";
  fs.define(Pid::lowest(), clauseFrom(d1, 8, 4));
  fs.define(Pid::lowest(), clauseFrom(d2, 8, 4));
  auto serialized = fs.serialize();
  EXPECT_EQ(serialized.first, Id::lowest());
  EXPECT_EQ(serialized.count, 2);
  // Verify serialized data can be deserialized back to matching facts
  binary::Input input(serialized.facts.bytes());
  for (size_t i = 0; i < serialized.count; ++i) {
    Pid type = Pid::invalid();
    Fact::Clause clause;
    Fact::deserialize(input, type, clause);
    EXPECT_EQ(type, Pid::lowest());
    EXPECT_EQ(clause.key_size, 4);
    EXPECT_EQ(clause.value_size, 4);
  }
}

TEST(FactSetTest, AllocatedMemoryReflectsFactData) {
  FactSet fs(Id::lowest());
  auto memEmpty = fs.allocatedMemory();
  unsigned char data[] = "some_key_datasome_value_data";
  fs.define(Pid::lowest(), clauseFrom(data, 27, 14));
  auto memOne = fs.allocatedMemory();
  EXPECT_GT(memOne, memEmpty);
  // Memory should grow by at least the size of the fact data
  EXPECT_GE(memOne - memEmpty, 27);
}

TEST(FactSetTest, PredicateStatsTracksFactCounts) {
  FactSet fs(Id::lowest());
  auto pid1 = Pid::lowest();
  auto pid2 = Pid::lowest() + 1;
  unsigned char d1[] = "a";
  unsigned char d2[] = "b";
  unsigned char d3[] = "cc";
  fs.define(pid1, clauseFrom(d1, 1, 1));
  fs.define(pid1, clauseFrom(d2, 1, 1));
  fs.define(pid2, clauseFrom(d3, 2, 2));
  auto stats = fs.predicateStats();
  auto* s1 = stats.lookup(pid1);
  auto* s2 = stats.lookup(pid2);
  ASSERT_NE(s1, nullptr);
  ASSERT_NE(s2, nullptr);
  EXPECT_EQ(s1->count, 2);
  EXPECT_EQ(s2->count, 1);
}

TEST(FactSetTest, SeekWithNonexistentTypeReturnsEmpty) {
  FactSet fs(Id::lowest());
  unsigned char d[] = "key";
  fs.define(Pid::lowest(), clauseFrom(d, 3, 3));
  unsigned char prefix[] = "x";
  auto iter = fs.seek(
      Pid::lowest() + 99,
      folly::ByteRange(prefix, static_cast<size_t>(0)),
      std::nullopt);
  EXPECT_FALSE(bool(iter->get()));
}

TEST(FactSetTest, FactsByDifferentPredicatesAreIndependent) {
  FactSet fs(Id::lowest());
  auto pid1 = Pid::lowest();
  auto pid2 = Pid::lowest() + 1;
  unsigned char d[] = "samekey";
  auto id1 = fs.define(pid1, clauseFrom(d, 7, 7));
  auto id2 = fs.define(pid2, clauseFrom(d, 7, 7));
  EXPECT_NE(id1, id2);
  EXPECT_EQ(fs.idByKey(pid1, folly::ByteRange(d, 7)), id1);
  EXPECT_EQ(fs.idByKey(pid2, folly::ByteRange(d, 7)), id2);
}

TEST(FactSetTest, LookupBelowStartingIdReturnsNotFound) {
  FactSet fs(Id::lowest() + 10);
  unsigned char d[] = "x";
  fs.define(Pid::lowest(), clauseFrom(d, 1, 1));
  EXPECT_EQ(fs.typeById(Id::lowest()), Pid::invalid());
  EXPECT_EQ(fs.typeById(Id::lowest() + 9), Pid::invalid());
  EXPECT_EQ(fs.typeById(Id::lowest() + 10), Pid::lowest());
  bool found = fs.factById(Id::lowest(), [](Pid, Fact::Clause) {});
  EXPECT_FALSE(found);
}

TEST(FactSetTest, SeekWithinSectionFullRangeWorks) {
  FactSet fs(Id::lowest());
  unsigned char d1[] = "abc";
  unsigned char d2[] = "abd";
  fs.define(Pid::lowest(), clauseFrom(d1, 3, 3));
  fs.define(Pid::lowest(), clauseFrom(d2, 3, 3));
  unsigned char prefix[] = "ab";
  auto iter = fs.seekWithinSection(
      Pid::lowest(),
      folly::ByteRange(prefix, 2),
      Id::invalid(),
      Id::lowest() + 100,
      std::nullopt);
  size_t count = 0;
  for (auto ref = iter->get(); ref; iter->next(), ref = iter->get()) {
    ++count;
  }
  EXPECT_EQ(count, 2);
}
