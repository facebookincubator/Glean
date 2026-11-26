/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <fmt/core.h>

#include "glean/rts/factset.h"
#include "glean/rts/inventory.h"
#include "glean/rts/ownership.h"
#include "glean/rts/ownership/expression.h"
#include "glean/rts/ownership/slice.h"

#include <gtest/gtest.h>
#include <algorithm>
#include <memory>
#include <optional>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

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
  // The {first id, number of sets} the set iterator reports, if not
  // {firstId_, sets_.size()}
  std::optional<std::pair<size_t, size_t>> reportedSizes_;
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
        std::pair<size_t, size_t> reportedSizes,
        std::vector<SetExpr<MutableOwnerSet>>& sets)
        : firstId_(firstId), reportedSizes_(reportedSizes), sets_(sets) {}

    std::pair<size_t, size_t> sizes() const override {
      return reportedSizes_;
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
    std::pair<size_t, size_t> reportedSizes_;
    uint32_t i_ = 0;
    std::vector<SetExpr<MutableOwnerSet>>& sets_;
  };

  return std::make_unique<MemorySetIterator>(
      firstId_,
      reportedSizes_.value_or(std::make_pair(firstId_, sets_.size())),
      sets_);
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

struct TestOwnershipUnitIterator final : OwnershipUnitIterator {
  struct Entry {
    UnitId unit;
    std::vector<OwnershipUnit::Ids> ids;
  };

  explicit TestOwnershipUnitIterator(std::vector<Entry> entries)
      : entries_(std::move(entries)) {}

  folly::Optional<OwnershipUnit> get() override {
    if (next_ == entries_.size()) {
      return folly::none;
    }
    const auto& entry = entries_[next_++];
    return OwnershipUnit{entry.unit, {entry.ids.data(), entry.ids.size()}};
  }

 private:
  std::vector<Entry> entries_;
  size_t next_ = 0;
};

std::shared_ptr<Subroutine> traversalFor(
    folly::Optional<std::pair<Id, Pid>> reference) {
  std::vector<uint64_t> code;
  size_t locals = 0;
  if (reference) {
    locals = 2;
    code = {
        static_cast<uint64_t>(Op::LoadConst),
        reference->first.toWord(),
        4,
        static_cast<uint64_t>(Op::LoadConst),
        reference->second.toWord(),
        5,
        static_cast<uint64_t>(Op::CallFun_2_0),
        0,
        4,
        5,
    };
  }
  code.push_back(static_cast<uint64_t>(Op::Ret));
  return std::make_shared<Subroutine>(
      std::move(code),
      4,
      0,
      locals,
      std::vector<uint64_t>{},
      std::vector<std::string>{});
}

Predicate predicateWithReference(
    Pid type,
    folly::Optional<std::pair<Id, Pid>> reference) {
  auto traverser = traversalFor(reference);
  return Predicate{type, "test.Predicate", 1, traverser, traverser};
}

UsetId ownerAt(const ComputedOwnership& ownership, Id fact) {
  UsetId owner = INVALID_USET;
  for (const auto& [start, candidate] : ownership.facts_) {
    if (start > fact) {
      break;
    }
    owner = candidate;
  }
  return owner;
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

TEST(OwnershipTest, EquivalentOwnershipExpressionsShareAnId) {
  Usets expressions(/* firstId */ 10);
  OwnershipExpressionBuilder builder(expressions);

  const auto first = builder.anyOf({2, 1, 2});
  const auto second = builder.anyOf({1, 2});
  const auto third = builder.anyOf({first, 1});

  EXPECT_EQ(first, second);
  EXPECT_EQ(first, third);
}

TEST(OwnershipTest, AlwaysSelectedUnitRepresentsUnrestrictedOwnership) {
  constexpr UnitId kAlwaysSelected = 0;
  constexpr UnitId kAlternativeA = 1;
  constexpr UnitId kAlternativeB = 2;
  constexpr UnitId kRequired = 3;
  constexpr UsetId kFirstExpression = 4;

  Usets expressions(kFirstExpression);
  OwnershipExpressionBuilder builder(expressions);
  const auto unrestrictedOwner = builder.anyOf({kAlwaysSelected});
  const auto alternativeOwner = builder.anyOf({kAlternativeA, kAlternativeB});
  const auto requiredOwner = builder.anyOf({kRequired});
  const auto constrainedOwner =
      builder.allOf({alternativeOwner, requiredOwner});

  TestOwnership ownership(
      kFirstExpression,
      expressions.toEliasFano(expressions.getNextId()),
      {unrestrictedOwner, constrainedOwner});
  Slices base({});

  const auto defaultSelection =
      slice(ownership, base, {kAlwaysSelected}, false);
  EXPECT_TRUE(defaultSelection->visible(ownership.getOwner(Id::fromWord(0))));
  EXPECT_FALSE(defaultSelection->visible(ownership.getOwner(Id::fromWord(1))));

  const auto extendedSelection = slice(
      ownership, base, {kAlwaysSelected, kAlternativeB, kRequired}, false);
  EXPECT_TRUE(extendedSelection->visible(ownership.getOwner(Id::fromWord(0))));
  EXPECT_TRUE(extendedSelection->visible(ownership.getOwner(Id::fromWord(1))));
}

TEST(OwnershipTest, SingletonOwnershipExpressionsPreserveVisibility) {
  constexpr UsetId kFirstExpression = 3;
  Usets expressions(kFirstExpression);
  std::vector<UsetId> owners;
  {
    OwnershipExpressionBuilder builder(expressions);
    const auto conjunction = builder.allOf({0, 1});
    const auto disjunction = builder.anyOf({0, 1});
    owners = {
        builder.anyOf({conjunction}),
        builder.allOf({disjunction}),
        builder.anyOf({conjunction, 2}),
        builder.allOf({disjunction, 2}),
        builder.allOf({0}),
        builder.anyOf({0})};
  }
  EXPECT_EQ(owners[4], owners[5]);
  const auto* singleton = expressions.lookupById(owners[4]);
  ASSERT_NE(singleton, nullptr);
  EXPECT_EQ(singleton->exp.op, Or);
  for (const auto owner : owners) {
    EXPECT_GE(owner, kFirstExpression);
  }
  TestOwnership ownership(
      kFirstExpression,
      expressions.toEliasFano(expressions.getNextId()),
      std::move(owners));
  Slices base({});

  for (const bool exclude : {false, true}) {
    for (unsigned selection = 0; selection < 8; ++selection) {
      SCOPED_TRACE(fmt::format("selection={} exclude={}", selection, exclude));
      std::vector<UnitId> units;
      for (UnitId unit = 0; unit < 3; ++unit) {
        if (selection & (1u << unit)) {
          units.push_back(unit);
        }
      }
      const bool a = ((selection & 1) != 0) != exclude;
      const bool b = ((selection & 2) != 0) != exclude;
      const bool c = ((selection & 4) != 0) != exclude;
      const std::vector<bool> expected = {
          a && b, a || b, (a && b) || c, (a || b) && c, a, a};
      const auto selected = slice(ownership, base, units, exclude);
      for (size_t fact = 0; fact < expected.size(); ++fact) {
        EXPECT_EQ(
            selected->visible(ownership.getOwner(Id::fromWord(fact))),
            expected[fact])
            << "fact=" << fact;
      }
    }
  }
}

TEST(OwnershipTest, OwnershipExpressionIdsRejectSentinels) {
  {
    Usets invalidRange(INVALID_USET);
    OwnershipExpressionBuilder invalidBuilder(invalidRange);
    EXPECT_THROW(invalidBuilder.anyOf({0}), std::runtime_error);
    EXPECT_EQ(invalidRange.getNextId(), INVALID_USET);
    EXPECT_EQ(invalidRange.statistics().promoted, 0);
  }

  constexpr UsetId kLastExpression = SPECIAL_USET - 1;
  Usets expressions(kLastExpression);
  OwnershipExpressionBuilder builder(expressions);
  const auto owner = builder.anyOf({0});
  ASSERT_EQ(owner, kLastExpression);

  EXPECT_THROW(builder.anyOf({1}), std::runtime_error);
  EXPECT_EQ(expressions.getNextId(), SPECIAL_USET);
  EXPECT_EQ(builder.anyOf({0}), owner);
  ASSERT_EQ(expressions.statistics().promoted, 1);

  TestOwnership ownership(
      kLastExpression,
      expressions.toEliasFano(expressions.getNextId()),
      {owner});
  Slices base({});
  EXPECT_FALSE(slice(ownership, base, {}, false)->visible(owner));
  EXPECT_TRUE(slice(ownership, base, {0}, false)->visible(owner));
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

TEST(OwnershipTest, SlicesBoundsIncludeEveryLayerInEitherOrder) {
  Slice lower(20, boost::dynamic_bitset<uint64_t>(2));
  Slice upper(30, boost::dynamic_bitset<uint64_t>(3));
  for (const auto& order : {
           std::vector<const Slice*>{&lower, &upper},
           std::vector<const Slice*>{&upper, &lower},
       }) {
    Slices slices(order);
    EXPECT_EQ(
        std::make_pair(slices.first(), slices.end()),
        std::make_pair(UsetId{20}, UsetId{33}));
    EXPECT_TRUE(slices.inRange(32));
  }
}

TEST(OwnershipTest, SlicesBoundsPreserveAZeroFirstId) {
  Slice lower(0, boost::dynamic_bitset<uint64_t>(2));
  Slice upper(10, boost::dynamic_bitset<uint64_t>(3));
  Slices slices({&lower, &upper});
  EXPECT_EQ(
      std::make_pair(slices.first(), slices.end()),
      std::make_pair(UsetId{0}, UsetId{13}));
  EXPECT_TRUE(slices.inRange(0));
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

TEST(OwnershipTest, ComputeOwnershipPropagatesOwnersTransitively) {
  FactSet facts(Id::lowest());
  unsigned char leafData[] = "leaf";
  unsigned char middleData[] = "middle";
  unsigned char topData[] = "top";
  const auto leaf = facts.define(
      Pid::lowest(), Fact::Clause::fromKey(folly::ByteRange(leafData, 4)));
  const auto middle = facts.define(
      Pid::lowest() + 1,
      Fact::Clause::fromKey(folly::ByteRange(middleData, 6)));
  const auto top = facts.define(
      Pid::lowest() + 2, Fact::Clause::fromKey(folly::ByteRange(topData, 3)));
  Inventory inventory({
      predicateWithReference(Pid::lowest(), folly::none),
      predicateWithReference(Pid::lowest() + 1, {{leaf, Pid::lowest()}}),
      predicateWithReference(Pid::lowest() + 2, {{middle, Pid::lowest() + 1}}),
  });
  TestOwnershipUnitIterator units({
      {0, {{leaf, leaf}}},
      {1, {{middle, middle}}},
      {2, {{top, top}}},
  });

  const auto ownership = computeOwnership(inventory, facts, nullptr, &units);

  const auto* leafOwner =
      ownership->sets_.lookupById(ownerAt(*ownership, leaf));
  const auto* middleOwner =
      ownership->sets_.lookupById(ownerAt(*ownership, middle));
  const auto* topOwner = ownership->sets_.lookupById(ownerAt(*ownership, top));
  ASSERT_NE(leafOwner, nullptr);
  ASSERT_NE(middleOwner, nullptr);
  ASSERT_NE(topOwner, nullptr);
  EXPECT_EQ(SetU32::to(leafOwner->exp.set), (std::set<uint32_t>{0, 1, 2}));
  EXPECT_EQ(SetU32::to(middleOwner->exp.set), (std::set<uint32_t>{1, 2}));
  EXPECT_EQ(SetU32::to(topOwner->exp.set), (std::set<uint32_t>{2}));
}

TEST(OwnershipTest, StoredUsetsKeepsTheIdsItIsGiven) {
  StoredUsets stored;

  const auto [first, firstAdded] =
      stored.add(std::make_unique<Uset>(SetU32::from({0, 1}), Or, 0), 12);
  const auto [duplicate, duplicateAdded] =
      stored.add(std::make_unique<Uset>(SetU32::from({0, 1}), Or, 0), 13);

  EXPECT_TRUE(firstAdded);
  EXPECT_EQ(first->id, 12u);
  EXPECT_FALSE(duplicateAdded);
  EXPECT_EQ(duplicate, first);
}

TEST(OwnershipTest, StoredUsetsTakesComputedSets) {
  Usets computed(10);
  auto* set = computed.add(SetU32::from({0, 1}));
  computed.promote(set);
  StoredUsets stored;

  stored.add(std::move(computed));

  Uset probe(SetU32::from({0, 1}), Or, 0);
  EXPECT_EQ(stored.lookup(&probe), set);
  EXPECT_EQ(set->id, 10u);
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

namespace {

// The fields serializeEliasFano writes before the data bytes.
struct EliasFanoHeader {
  uint64_t size;
  uint64_t numLowerBits;
  uint64_t upperSizeBytes;
  uint64_t skipPointers;
  uint64_t forwardPointers;
  uint64_t lower;
  uint64_t upper;
};

// Serializes a set into a heap buffer with the 8 bytes of padding that
// storage/common.h adds to stored sets, and passes the deserialized set to
// `use`.
template <typename F>
void withStoredSet(
    const EliasFanoHeader& header,
    const std::vector<uint8_t>& data,
    F&& use) {
  binary::Output out;
  out.nat(header.size);
  out.nat(header.numLowerBits);
  out.nat(header.upperSizeBytes);
  out.nat(header.skipPointers);
  out.nat(header.forwardPointers);
  out.nat(header.lower);
  out.nat(header.upper);
  out.put(folly::ByteRange(data.data(), data.size()));
  constexpr size_t kPadding = 8;
  auto stored = std::make_unique<uint8_t[]>(out.size() + kPadding);
  std::copy(out.data(), out.data() + out.size(), stored.get());
  binary::Input in(stored.get(), out.size());
  use(deserializeEliasFano(in));
}

void decodeStoredSet(
    const EliasFanoHeader& header,
    const std::vector<uint8_t>& data) {
  withStoredSet(
      header, data, [](const OwnerSet& set) { SetU32::fromEliasFano(set); });
}

} // namespace

TEST(OwnershipTest, DeserializeRejectsUpperOffsetPastTheData) {
  EXPECT_THROW(
      decodeStoredSet(
          {.size = 1, .numLowerBits = 0, .upperSizeBytes = 1, .upper = 9},
          {0x01}),
      std::runtime_error);
}

TEST(OwnershipTest, DeserializeRejectsLowerBitsPastTheData) {
  // Two elements of 31 lower bits each need 8 bytes of lower bits.
  EXPECT_THROW(
      decodeStoredSet(
          {.size = 2,
           .numLowerBits = 31,
           .upperSizeBytes = 1,
           .lower = 1,
           .upper = 0},
          {0x03}),
      std::runtime_error);
}

TEST(OwnershipTest, DeserializeRejectsLowerBitsOneByteShort) {
  // Two elements of 31 lower bits need 8 bytes; 7 follow the lower offset.
  EXPECT_THROW(
      decodeStoredSet(
          {.size = 2,
           .numLowerBits = 31,
           .upperSizeBytes = 1,
           .lower = 1,
           .upper = 0},
          {0x03, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff}),
      std::runtime_error);
}

TEST(OwnershipTest, DeserializeRejectsMoreElementsThanUpperBitsHold) {
  EXPECT_THROW(
      decodeStoredSet(
          {.size = 2, .numLowerBits = 0, .upperSizeBytes = 1}, {0x01}),
      std::runtime_error);
}

TEST(OwnershipTest, DeserializeRejectsFewerElementsThanUpperBitsHold) {
  // The reader bounds skipTo by the number of 0-bits it infers from `size`;
  // with 128 1-bits and size 1 it infers 127 that are not there.
  using Reader = folly::compression::EliasFanoReader<
      folly::compression::EliasFanoEncoder<uint32_t, uint32_t>>;
  EXPECT_THROW(
      withStoredSet(
          {.size = 1, .numLowerBits = 0, .upperSizeBytes = 16},
          std::vector<uint8_t>(16, 0xff),
          [](const OwnerSet& set) { Reader(set).skipTo(127); }),
      std::runtime_error);
}

TEST(OwnershipTest, DeserializeRejectsMalformedHeaders) {
  const std::vector<std::pair<std::string, EliasFanoHeader>> headers = {
      {"skip pointers past the data",
       {.size = 1, .upperSizeBytes = 1, .skipPointers = 2}},
      {"forward pointers past the data",
       {.size = 1, .upperSizeBytes = 1, .forwardPointers = 2}},
      {"lower bits offset past the data",
       {.size = 1, .upperSizeBytes = 1, .lower = 2}},
      {"upper bits longer than the data", {.size = 1, .upperSizeBytes = 2}},
      {"32 lower bits", {.size = 1, .numLowerBits = 32, .upperSizeBytes = 1}},
      {"more than 2^32 elements",
       {.size = uint64_t{1} << 32, .upperSizeBytes = 1}},
  };
  for (const auto& [what, header] : headers) {
    EXPECT_THROW(decodeStoredSet(header, {0x01}), std::runtime_error) << what;
  }
}

TEST(OwnershipTest, DeserializeRejectsTruncatedHeader) {
  const uint8_t truncated[] = {0x01, 0x00};
  binary::Input in(truncated, sizeof(truncated));
  EXPECT_THROW(deserializeEliasFano(in), std::runtime_error);
}

TEST(OwnershipTest, DeserializeAcceptsEmptySetWithUpperBits) {
  // Upper bits with no 1-bits describe an empty set, however many there are.
  std::set<uint32_t> decoded = {0};

  withStoredSet(
      {.size = 0, .numLowerBits = 0, .upperSizeBytes = 2},
      {0x00, 0x00},
      [&](const OwnerSet& set) {
        decoded = SetU32::to(SetU32::fromEliasFano(set));
      });

  EXPECT_EQ(decoded, std::set<uint32_t>{});
}

TEST(OwnershipTest, SliceRejectsOrSetReferringPastTheSets) {
  // One set, 10, whose element 74 is in the range of sets but past them.
  TestOwnership ownership(10, makeSetExprs({{Or, {74}}}), {});

  EXPECT_THROW(slice(ownership, Slices({}), {}, false), std::runtime_error);
}

TEST(OwnershipTest, SliceRejectsAndSetReferringPastTheSets) {
  TestOwnership ownership(10, makeSetExprs({{And, {74}}}), {});

  EXPECT_THROW(slice(ownership, Slices({}), {}, false), std::runtime_error);
}

TEST(OwnershipTest, SliceRejectsSetReferringToTheNextSetId) {
  // Set 10's element 11 is the first id past the one stored set.
  TestOwnership ownership(10, makeSetExprs({{Or, {11}}}), {});

  EXPECT_THROW(slice(ownership, Slices({}), {}, false), std::runtime_error);
}

TEST(OwnershipTest, SliceRejectsSetIdsPastTheReportedSets) {
  // The iterator yields sets 10..74 but reports only set 10.
  std::vector<SetExpr<MutableOwnerSet>> sets;
  sets.reserve(65);
  for (int i = 0; i < 65; i++) {
    sets.push_back(makeSetExpr(Or, {1}));
  }
  TestOwnership ownership(10, std::move(sets), {});
  ownership.reportedSizes_ = {10, 1};

  EXPECT_THROW(slice(ownership, Slices({}), {}, false), std::runtime_error);
}

TEST(OwnershipTest, SliceRejectsSetIdsBeforeTheReportedSets) {
  // The iterator yields set 10 but reports only set 11.
  TestOwnership ownership(10, makeSetExprs({{Or, {1}}}), {});
  ownership.reportedSizes_ = {11, 1};

  EXPECT_THROW(slice(ownership, Slices({}), {}, false), std::runtime_error);
}

TEST(OwnershipTest, ComputeOwnershipRejectsOwnedFactsBelowANonStackedDb) {
  FactSet facts(Id::lowest() + 100);
  unsigned char key[] = "fact";
  facts.define(Pid::lowest(), Fact::Clause::fromKey(folly::ByteRange(key, 4)));
  Inventory inventory({predicateWithReference(Pid::lowest(), folly::none)});
  // Two ids below the first fact: flatten() drops a single-id range.
  TestOwnershipUnitIterator units({{0, {{Id::lowest(), Id::lowest() + 1}}}});

  EXPECT_THROW(
      computeOwnership(inventory, facts, nullptr, &units), std::runtime_error);
}

TEST(OwnershipTest, ComputeOwnershipRejectsReferencesBelowANonStackedDb) {
  // A local fact's reference, not an owned interval, names a fact below the
  // DB's first fact.
  FactSet facts(Id::lowest() + 100);
  unsigned char keys[] = "ab";
  const auto f0 = facts.define(
      Pid::lowest(), Fact::Clause::fromKey(folly::ByteRange(keys, 1)));
  const auto f1 = facts.define(
      Pid::lowest() + 1, Fact::Clause::fromKey(folly::ByteRange(keys + 1, 1)));
  Inventory inventory({
      predicateWithReference(Pid::lowest(), folly::none),
      predicateWithReference(
          Pid::lowest() + 1, {{Id::lowest(), Pid::lowest()}}),
  });
  TestOwnershipUnitIterator units({{0, {{f0, f1}}}});

  EXPECT_THROW(
      computeOwnership(inventory, facts, nullptr, &units), std::runtime_error);
}

TEST(OwnershipTest, ComputeOwnershipFollowsOwnedFactsIntoTheBase) {
  FactSet base(Id::lowest());
  unsigned char baseKey[] = "base";
  const auto baseFact = base.define(
      Pid::lowest(), Fact::Clause::fromKey(folly::ByteRange(baseKey, 4)));
  FactSet facts(base.firstFreeId());
  unsigned char key[] = "fact";
  const auto fact = facts.define(
      Pid::lowest(), Fact::Clause::fromKey(folly::ByteRange(key, 4)));
  Inventory inventory({predicateWithReference(Pid::lowest(), folly::none)});
  TestOwnershipUnitIterator units({{0, {{baseFact, fact}}}});

  const auto ownership = computeOwnership(inventory, facts, &base, &units);

  const auto* baseOwner =
      ownership->sets_.lookupById(ownerAt(*ownership, baseFact));
  ASSERT_NE(baseOwner, nullptr);
  EXPECT_EQ(SetU32::to(baseOwner->exp.set), (std::set<uint32_t>{0}));
}

TEST(OwnershipTest, ComputeOwnershipRejectsUnsortedOwnedIntervals) {
  // With these ids, unit 1's second interval revisits a leaf that its first
  // interval turned into a link.
  FactSet facts(Id::lowest() + 100);
  unsigned char keys[] = "abc";
  std::vector<Id> ids;
  ids.reserve(3);
  for (size_t i = 0; i < 3; i++) {
    ids.push_back(facts.define(
        Pid::lowest(), Fact::Clause::fromKey(folly::ByteRange(keys + i, 1))));
  }
  Inventory inventory({predicateWithReference(Pid::lowest(), folly::none)});
  // More units follow the invalid one than the unit queue holds.
  std::vector<TestOwnershipUnitIterator::Entry> entries = {
      {0, {{ids[0], ids[2]}}},
      {1, {{ids[0], ids[2]}, {ids[1], ids[1]}}},
  };
  for (UnitId unit = 2; unit < 300; unit++) {
    entries.push_back({unit, {{ids[0], ids[0]}}});
  }
  TestOwnershipUnitIterator units(std::move(entries));

  EXPECT_THROW(
      computeOwnership(inventory, facts, nullptr, &units), std::runtime_error);
}

TEST(OwnershipTest, ComputeOwnershipRejectsIntervalsSharingAnId) {
  FactSet facts(Id::lowest() + 100);
  unsigned char keys[] = "abc";
  std::vector<Id> ids;
  ids.reserve(3);
  for (size_t i = 0; i < 3; i++) {
    ids.push_back(facts.define(
        Pid::lowest(), Fact::Clause::fromKey(folly::ByteRange(keys + i, 1))));
  }
  Inventory inventory({predicateWithReference(Pid::lowest(), folly::none)});
  // Unit 1's closed intervals both contain ids[1].
  TestOwnershipUnitIterator units({
      {0, {{ids[0], ids[2]}}},
      {1, {{ids[0], ids[1]}, {ids[1], ids[2]}}},
  });

  EXPECT_THROW(
      computeOwnership(inventory, facts, nullptr, &units), std::runtime_error);
}

TEST(OwnershipTest, ComputeOwnershipRejectsInvertedOwnedInterval) {
  FactSet facts(Id::lowest() + 100);
  unsigned char keys[] = "ab";
  const auto first = facts.define(
      Pid::lowest(), Fact::Clause::fromKey(folly::ByteRange(keys, 1)));
  const auto second = facts.define(
      Pid::lowest(), Fact::Clause::fromKey(folly::ByteRange(keys + 1, 1)));
  Inventory inventory({predicateWithReference(Pid::lowest(), folly::none)});
  TestOwnershipUnitIterator units({{0, {{second, first}}}});

  EXPECT_THROW(
      computeOwnership(inventory, facts, nullptr, &units), std::runtime_error);
}

TEST(OwnershipTest, ComputeOwnershipAcceptsSortedDisjointIntervals) {
  FactSet facts(Id::lowest() + 100);
  unsigned char keys[] = "abc";
  std::vector<Id> ids;
  ids.reserve(3);
  for (size_t i = 0; i < 3; i++) {
    ids.push_back(facts.define(
        Pid::lowest(), Fact::Clause::fromKey(folly::ByteRange(keys + i, 1))));
  }
  Inventory inventory({predicateWithReference(Pid::lowest(), folly::none)});
  TestOwnershipUnitIterator units({{0, {{ids[0], ids[0]}, {ids[2], ids[2]}}}});

  const auto ownership = computeOwnership(inventory, facts, nullptr, &units);

  EXPECT_NE(ownerAt(*ownership, ids[0]), INVALID_USET);
  EXPECT_EQ(ownerAt(*ownership, ids[1]), INVALID_USET);
  EXPECT_NE(ownerAt(*ownership, ids[2]), INVALID_USET);
}
