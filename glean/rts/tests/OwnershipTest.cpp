// Copyright (c) Facebook, Inc. and its affiliates.

#include <fmt/core.h>

#include "glean/rts/ownership.h"
#include "glean/rts/ownership/slice.h"

#include <gtest/gtest.h>

using namespace facebook::glean::rts;

namespace {

struct TestOwnership final : Ownership {
  ~TestOwnership() override {
     for (auto &set: sets_) {
       set.set.free();
     }
   }

  TestOwnership(UsetId firstId,
                std::vector<SetExpr<MutableOwnerSet>>&& sets,
                std::vector<UsetId>&& facts) :
      firstId_(firstId), sets_(std::move(sets)), facts_(std::move(facts)) {}

  UsetId nextSetId() override {
    return firstId_ + sets_.size();
  }

  UsetId getOwner(Id id) override {
    // facts_.size() might be smaller than the total number of facts
    // if there were some unowned facts at the end, so we need a
    // bounds check.
    return id.toWord() < facts_.size() ? facts_[id.toWord()] : INVALID_USET;
  }

  std::unique_ptr<OwnershipSetIterator> getSetIterator() override;

  UsetId lookupSet(Uset*) override {
    LOG(FATAL) << "unimplemented: lookupSet";
  }

  UsetId firstId_;
  std::vector<SetExpr<MutableOwnerSet>> sets_; // Sets, indexed by UsetId
  std::vector<UsetId> facts_; // Owner set for each fact
};

std::unique_ptr<OwnershipSetIterator> TestOwnership::getSetIterator() {
  struct MemorySetIterator : OwnershipSetIterator {
    MemorySetIterator(UsetId firstId,
                      std::vector<SetExpr<MutableOwnerSet>>& sets) :
      firstId_(firstId), sets_(sets) {}

    std::pair<size_t,size_t> sizes() const override {
      return { firstId_, sets_.size() };
    }

    folly::Optional<std::pair<UnitId,SetExpr<const OwnerSet*>>> get() override {
      if (i_ >= sets_.size()) {
        return folly::none;
      } else {
        uint32_t i = i_++;
        assert(!sets_.empty()); // be gone, linter
        ownerset = sets_[i].set;
        return std::pair<UnitId,SetExpr<const OwnerSet*>>(
            firstId_ + i, { sets_[i].op, &ownerset });
      }
    }

    OwnerSet ownerset;
    UsetId firstId_;
    uint32_t i_ = 0;
    std::vector<SetExpr<MutableOwnerSet>>& sets_;
  };

  return std::make_unique<MemorySetIterator>(firstId_, sets_);
}


void checkVisibility(
    TestOwnership &ownership,
    uint32_t firstUsetId,
    uint32_t numSets,
    std::vector<UnitId> units,
    bool exclude) {
  std::set set(units.begin(), units.end());

  auto sl = slice(ownership, units, exclude);

  using Reader = folly::compression::EliasFanoReader<
      folly::compression::EliasFanoEncoder<uint32_t, uint32_t>>;

  auto anyMember = [&](Reader &reader, bool member) {
    bool any = false;
    while (reader.next() && reader.value() < firstUsetId) {
      if (member == (set.find(reader.value()) != set.end())) {
        any = true;
      }
    }
    return any;
  };

  auto allMembers = [&](Reader &reader, bool member) {
    bool all = true;
    while (reader.next() && reader.value() < firstUsetId) {
      if (member != (set.find(reader.value()) != set.end())) {
        all = false;
      }
    }
    return all;
  };

  auto orVisible = [&](Reader &reader) {
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

  auto andVisible = [&](Reader &reader) {
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
    SCOPED_TRACE(fmt::format("set {} is {}", setId,
                            visible ? "visible" : "invisible"));
    auto& exp = ownership.sets_[setId - firstUsetId];
    Reader reader(exp.set);
    switch (exp.op) {
      case Or:
        if (exclude) {
          if (visible) {
            // at least one owner is not excluded
            EXPECT_TRUE(
                anyMember(reader, false) ||
                orVisible(reader));
          } else {
            // the owner set should be all excluded
            EXPECT_TRUE(
                allMembers(reader, true) &&
                !orVisible(reader));
          }
        } else /* include */ {
          if (visible) {
            // at least one owner is included
            EXPECT_TRUE(
                anyMember(reader, true) ||
                orVisible(reader));
          } else {
            // all owners are not in the set
            EXPECT_TRUE(
                allMembers(reader, false) &&
                !orVisible(reader));
          }
        }
        break;

      case And:
        if (exclude) {
          if (visible) {
            // all owners are not in the set
            EXPECT_TRUE(
                allMembers(reader, false) &&
                andVisible(reader));
          } else {
            // at least one owner is excluded
            EXPECT_TRUE(
                anyMember(reader, true) ||
                !andVisible(reader));
          }
        } else /* include */ {
          if (visible) {
            // all owners should be included
            EXPECT_TRUE(
                allMembers(reader, true) &&
                andVisible(reader));
          } else {
            // at least one owner is not included
            EXPECT_TRUE(
                anyMember(reader, false) ||
                !andVisible(reader));
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
    for (uint32_t j = i+1; j < numSets; j++) {
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

}


TEST(OwnershipTest, SliceTest) {
  std::vector<UnitId> units = {0,1,2};

  auto usets = buildExampleSets(units);
  auto firstUsetId = usets.getFirstId();
  auto sets = usets.toEliasFano();
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
  checkVisibility(ownership, firstUsetId, numSets, {0,2}, true);
  checkVisibility(ownership, firstUsetId, numSets, {0,2}, false);
  checkVisibility(ownership, firstUsetId, numSets, {0,1,2}, true);
  checkVisibility(ownership, firstUsetId, numSets, {0,1,2}, false);
}
