/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <fmt/core.h>

#include "glean/rts/ownership.h"
#include "glean/rts/ownership/slice.h"

#include <gtest/gtest.h>

#include <rapidcheck.h>
#include <rapidcheck/gtest.h>

using namespace facebook::glean;
using namespace facebook::glean::rts;

namespace {

struct TestOwnership final : Ownership {
  ~TestOwnership() override {
    for (auto& set : sets_) {
      set.set.free();
    }
  }

  TestOwnership(
      UsetId firstId,
      std::vector<SetExpr<MutableOwnerSet>>&& sets,
      std::vector<UsetId>&& facts)
      : firstId_(firstId), sets_(std::move(sets)), facts_(std::move(facts)) {}

  UsetId nextSetId() override {
    return firstId_ + sets_.size();
  }

  UsetId getOwner(Id id) {
    // facts_.size() might be smaller than the total number of facts
    // if there were some unowned facts at the end, so we need a
    // bounds check.
    return id.toWord() < facts_.size() ? facts_[id.toWord()] : INVALID_USET;
  }

  std::unique_ptr<OwnershipSetIterator> getSetIterator() override;

  UsetId lookupSet(Uset*) override {
    LOG(FATAL) << "unimplemented: lookupSet";
  }

  folly::Optional<SetExpr<SetU32>> getUset(UsetId) override {
    LOG(FATAL) << "unimplemented: getUset";
  }

  folly::Optional<UnitId> getUnitId(folly::ByteRange) override {
    LOG(FATAL) << "unimplemented: getUnitId";
  }

  OwnershipStats getStats() override {
    LOG(FATAL) << "unimplemented: getStats";
  }

  UsetId firstId_;
  std::vector<SetExpr<MutableOwnerSet>> sets_; // Sets, indexed by UsetId
  std::vector<UsetId> facts_; // Owner set for each fact
};

SetExpr<MutableOwnerSet> makeSetExpr(SetOp op, std::set<UsetId> members) {
  const auto upper = members.empty() ? 0 : *members.rbegin() + 1;
  return {op, SetU32::from(members).toEliasFano(upper)};
}

std::vector<SetExpr<MutableOwnerSet>> makeSetExprs(
    std::initializer_list<std::pair<SetOp, std::set<UsetId>>> specs) {
  std::vector<SetExpr<MutableOwnerSet>> result;
  result.reserve(specs.size());
  for (const auto& [op, members] : specs) {
    result.push_back(makeSetExpr(op, members));
  }
  return result;
}

std::unique_ptr<OwnershipSetIterator> TestOwnership::getSetIterator() {
  struct MemorySetIterator : OwnershipSetIterator {
    MemorySetIterator(
        UsetId firstId,
        std::vector<SetExpr<MutableOwnerSet>>& sets)
        : firstId_(firstId), sets_(sets) {}

    std::pair<size_t, size_t> sizes() const override {
      return {firstId_, sets_.size()};
    }

    folly::Optional<std::pair<UnitId, SetExpr<const OwnerSet*>>> get()
        override {
      if (i_ >= sets_.size()) {
        return folly::none;
      } else {
        uint32_t i = i_++;
        assert(!sets_.empty()); // be gone, linter
        ownerset = sets_[i].set;
        return std::pair<UnitId, SetExpr<const OwnerSet*>>(
            firstId_ + i, {sets_[i].op, &ownerset});
      }
    }

    OwnerSet ownerset;
    UsetId firstId_;
    uint32_t i_ = 0;
    std::vector<SetExpr<MutableOwnerSet>>& sets_;
  };

  return std::make_unique<MemorySetIterator>(firstId_, sets_);
}

std::vector<bool> visibleUsets(const Slice& slice, UsetId first, UsetId end) {
  std::vector<bool> result;
  for (UsetId id = first; id < end; ++id) {
    result.push_back(slice.visible(id));
  }
  return result;
}

void checkVisibility(
    TestOwnership& ownership,
    uint32_t firstUsetId,
    uint32_t numSets,
    std::vector<UnitId> units,
    bool exclude) {
  std::set set(units.begin(), units.end());

  Slices base{{}};
  auto sl = slice(ownership, base, units, exclude);

  using Reader = folly::compression::EliasFanoReader<
      folly::compression::EliasFanoEncoder<uint32_t, uint32_t>>;

  auto anyMember = [&](Reader& reader, bool member) {
    bool any = false;
    while (reader.next() && reader.value() < firstUsetId) {
      if (member == (set.find(reader.value()) != set.end())) {
        any = true;
      }
    }
    return any;
  };

  auto allMembers = [&](Reader& reader, bool member) {
    bool all = true;
    while (reader.next() && reader.value() < firstUsetId) {
      if (member != (set.find(reader.value()) != set.end())) {
        all = false;
      }
    }
    return all;
  };

  auto orVisible = [&](Reader& reader) {
    if (!reader.valid()) {
      return false;
    }
    assert(reader.value() >= firstUsetId);
    bool any = false;
    do {
      if (sl->visible(reader.value())) {
        any = true;
      }
    } while (reader.next());
    return any;
  };

  auto andVisible = [&](Reader& reader) {
    if (!reader.valid()) {
      return true;
    }
    assert(reader.value() >= firstUsetId);
    bool all = true;
    do {
      if (!sl->visible(reader.value())) {
        all = false;
      }
    } while (reader.next());
    return all;
  };

  for (uint32_t i = 0; i < numSets; i++) {
    auto setId = ownership.getOwner(Id::fromWord(i));
    bool visible = sl->visible(setId);
    SCOPED_TRACE(
        fmt::format("set {} is {}", setId, visible ? "visible" : "invisible"));
    auto& exp = ownership.sets_[setId - firstUsetId];
    Reader reader(exp.set);
    switch (exp.op) {
      case Or:
        if (exclude) {
          if (visible) {
            // at least one owner is not excluded
            EXPECT_TRUE(anyMember(reader, false) || orVisible(reader));
          } else {
            // the owner set should be all excluded
            EXPECT_TRUE(allMembers(reader, true) && !orVisible(reader));
          }
        } else /* include */ {
          if (visible) {
            // at least one owner is included
            EXPECT_TRUE(anyMember(reader, true) || orVisible(reader));
          } else {
            // all owners are not in the set
            EXPECT_TRUE(allMembers(reader, false) && !orVisible(reader));
          }
        }
        break;

      case And:
        if (exclude) {
          if (visible) {
            // all owners are not in the set
            EXPECT_TRUE(allMembers(reader, false) && andVisible(reader));
          } else {
            // at least one owner is excluded
            EXPECT_TRUE(anyMember(reader, true) || !andVisible(reader));
          }
        } else /* include */ {
          if (visible) {
            // all owners should be included
            EXPECT_TRUE(allMembers(reader, true) && andVisible(reader));
          } else {
            // at least one owner is not included
            EXPECT_TRUE(anyMember(reader, false) || !andVisible(reader));
          }
        }
        break;
    }
  }
}

Usets buildExampleSets(std::vector<UnitId> units) {
  auto firstUsetId = units.size();
  Usets usets(firstUsetId);

  auto addSet = [&](SetOp op, SetU32 set) {
    auto uset = new Uset(std::move(set), op, 0);
    auto p = usets.add(uset);
    EXPECT_EQ(p, uset);
    usets.promote(p);
    return p;
  };

  // form sets of all singletons and pairs of units
  for (uint32_t i = 0; i < units.size(); i++) {
    for (uint32_t j = i; j < units.size(); j++) {
      SetU32 s;
      if (i == j) {
        s.append(units[i]);
      } else {
        s.append(units[i]);
        s.append(units[j]);
      }
      auto p1 = addSet(Or, s);
      LOG(INFO) << fmt::format("set {} is {} || {}", p1->id, i, j);
      auto p2 = addSet(And, s);
      LOG(INFO) << fmt::format("set {} is {} && {}", p2->id, i, j);
    }
  }

  // add the set of all units
  SetU32 s;
  for (uint32_t i = 0; i < units.size(); i++) {
    s.append(units[i]);
  }
  addSet(Or, s);
  addSet(And, s);

  // Now add expressions involving the sets above
  //   A && B
  //   A || B
  // for all distinct sets A,B
  uint32_t numSets = usets.statistics().promoted;
  for (uint32_t i = units.size(); i < numSets; i++) {
    for (uint32_t j = i + 1; j < numSets; j++) {
      SetU32 t;
      t.append(i);
      t.append(j);
      auto p1 = addSet(Or, t);
      LOG(INFO) << fmt::format("set {} is {} || {}", p1->id, i, j);
      auto p2 = addSet(And, t);
      LOG(INFO) << fmt::format("set {} is {} && {}", p2->id, i, j);
    }
  }

  // Finally, conjunction/disjunction of all the exprs so far
  numSets = usets.statistics().promoted;
  SetU32 t;
  for (uint32_t i = units.size(); i < numSets; i++) {
    t.append(i);
  }
  addSet(Or, t);
  addSet(And, t);

  return usets;
}

} // namespace

TEST(OwnershipTest, SliceTest) {
  std::vector<UnitId> units = {0, 1, 2};

  auto usets = buildExampleSets(units);
  auto firstUsetId = usets.getFirstId();
  auto sets = usets.toEliasFano(usets.getFirstId() + usets.size());
  uint32_t numSets = usets.statistics().promoted;

  // One fact with each different set
  std::vector<UsetId> facts(numSets);
  for (uint32_t i = 0; i < numSets; i++) {
    facts[i] = i + firstUsetId;
  }

  TestOwnership ownership(firstUsetId, std::move(sets), std::move(facts));

  // test visibility of facts with various slices
  checkVisibility(ownership, firstUsetId, numSets, {}, true);
  checkVisibility(ownership, firstUsetId, numSets, {}, false);
  checkVisibility(ownership, firstUsetId, numSets, {0}, true);
  checkVisibility(ownership, firstUsetId, numSets, {0}, false);
  checkVisibility(ownership, firstUsetId, numSets, {0, 2}, true);
  checkVisibility(ownership, firstUsetId, numSets, {0, 2}, false);
  checkVisibility(ownership, firstUsetId, numSets, {0, 1, 2}, true);
  checkVisibility(ownership, firstUsetId, numSets, {0, 1, 2}, false);
}

TEST(OwnershipTest, SlicesRequireEveryCoveringSliceToBeVisible) {
  boost::dynamic_bitset<uint64_t> ownershipBits(4);
  ownershipBits.set(0);
  ownershipBits.set(2);
  ownershipBits.set(3);
  Slice ownership(10, ownershipBits);

  boost::dynamic_bitset<uint64_t> aclBits(4);
  aclBits.set(0);
  aclBits.set(1);
  aclBits.set(3);
  Slice acl(10, aclBits);

  Slices slices({&ownership, &acl});

  EXPECT_EQ(slices.first(), 10);
  EXPECT_EQ(slices.end(), 14);
  EXPECT_TRUE(slices.visible(10));
  EXPECT_FALSE(slices.visible(11));
  EXPECT_FALSE(slices.visible(12));
  EXPECT_TRUE(slices.visible(13));
  EXPECT_FALSE(slices.visible(14));
  EXPECT_FALSE(slices.visible(INVALID_USET));
}

TEST(OwnershipTest, SlicesRangeCanContainGapsThatAreNotVisible) {
  boost::dynamic_bitset<uint64_t> lowerBits(2);
  lowerBits.set(0);
  Slice lower(20, lowerBits);

  boost::dynamic_bitset<uint64_t> upperBits(1);
  upperBits.set(0);
  Slice upper(30, upperBits);

  Slices slices({&upper, &lower});

  EXPECT_EQ(slices.first(), 20);
  EXPECT_EQ(slices.end(), 31);
  EXPECT_TRUE(slices.inRange(25));
  EXPECT_FALSE(slices.visible(25));
}

TEST(OwnershipTest, EmptyOwnershipUsesBaseEndAsEmptySliceStart) {
  boost::dynamic_bitset<uint64_t> baseBits(3);
  baseBits.set(0);
  Slice baseSlice(40, baseBits);
  Slices base({&baseSlice});

  TestOwnership ownership(
      100, std::vector<SetExpr<MutableOwnerSet>>{}, std::vector<UsetId>{});

  const auto result = slice(ownership, base, {41}, false);

  EXPECT_TRUE(result->empty());
  EXPECT_EQ(result->first(), base.end());
  EXPECT_EQ(result->end(), base.end());
}

TEST(OwnershipTest, SliceUsesVisibleBaseSetsAndCurrentUnits) {
  boost::dynamic_bitset<uint64_t> baseBits(2);
  baseBits.set(0);
  Slice baseSlice(50, baseBits);
  Slices base({&baseSlice});

  TestOwnership ownership(
      100,
      makeSetExprs({
          {Or, {50}},
          {Or, {51, 60}},
          {And, {50, 60}},
          {And, {51, 60}},
      }),
      std::vector<UsetId>{});

  const auto result = slice(ownership, base, {60}, false);

  EXPECT_EQ(
      visibleUsets(*result, 100, 104),
      std::vector<bool>({
          true,
          true,
          true,
          false,
      }));
}

TEST(OwnershipTest, SliceExclusionPropagatesThroughCurrentDbSets) {
  Slices base({});
  TestOwnership ownership(
      10,
      makeSetExprs({
          {Or, {1, 2}},
          {And, {1, 2}},
          {Or, {10, 11}},
          {And, {10, 11}},
      }),
      std::vector<UsetId>{});

  const auto result = slice(ownership, base, {1}, true);

  EXPECT_EQ(
      visibleUsets(*result, 10, 14),
      std::vector<bool>({
          true,
          false,
          true,
          false,
      }));
}

TEST(OwnershipTest, SliceSerializationPreservesVisibleMembers) {
  boost::dynamic_bitset<uint64_t> bits(70);
  bits.set(0);
  bits.set(3);
  bits.set(65);
  Slice original(200, bits);

  binary::Output output;
  original.serialize(output);
  binary::Input input(output.data(), output.size());

  const auto restored = Slice::deserialize(input);

  EXPECT_EQ(restored->first(), 200);
  EXPECT_TRUE(restored->visible(200));
  EXPECT_FALSE(restored->visible(201));
  EXPECT_TRUE(restored->visible(203));
  EXPECT_TRUE(restored->visible(265));
  EXPECT_FALSE(restored->visible(266));
}

struct SetSerializationTest : testing::Test {};

RC_GTEST_PROP(SetSerializationTest, testSerialization, ()) {
  const auto set = *rc::gen::nonEmpty(rc::gen::arbitrary<std::set<uint32_t>>());
  SetU32 a = SetU32::from(set);
  MutableOwnerSet b = a.toEliasFano(a.upper() + 1);
  binary::Output o;
  serializeEliasFano(o, b);
  auto size = o.size();
  o.expect(8);
  binary::Input i(o.data(), size);
  OwnerSet c = deserializeEliasFano(i);
  SetU32 d = SetU32::fromEliasFano(c);
  b.free();
  RC_ASSERT(set == SetU32::to(d));
}
