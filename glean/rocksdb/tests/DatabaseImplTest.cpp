/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <gtest/gtest.h>

#include <cstdint>
#include <cstdio>
#include <map>
#include <memory>
#include <optional>
#include <set>
#include <stdexcept>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include <folly/Range.h>
#include <folly/init/Init.h>
#include <folly/testing/TestUtil.h>
#include <rocksdb/utilities/stackable_db.h>

#include "glean/rocksdb/container-impl.h"
#include "glean/rocksdb/database-impl.h"
#include "glean/rocksdb/rocksdb.h"
#include "glean/rts/cache.h"
#include "glean/rts/ownership/expression.h"
#include "glean/rts/stacked.h"
#include "glean/storage/common.h"

namespace facebook::glean::rocks {

namespace {

folly::ByteRange bytes(folly::StringPiece value) {
  return {reinterpret_cast<const unsigned char*>(value.data()), value.size()};
}

rts::Id defineFact(
    rts::FactSet& facts,
    rts::Pid type,
    folly::StringPiece key,
    folly::StringPiece value) {
  const std::string clause = key.str() + value.str();
  return facts.define(type, rts::Fact::Clause::from(bytes(clause), key.size()));
}

std::vector<rts::Id> collectIds(std::unique_ptr<rts::FactIterator> iterator) {
  std::vector<rts::Id> ids;
  for (auto fact = iterator->get(); fact;
       iterator->next(), fact = iterator->get()) {
    ids.push_back(fact.id);
  }
  return ids;
}

struct StoredFact {
  rts::Pid type = rts::Pid::invalid();
  std::string key;
  std::string value;
};

class StatsTestDB final : public rocksdb::StackableDB {
 public:
  explicit StatsTestDB(std::unique_ptr<rocksdb::DB> database)
      : StackableDB(std::move(database)) {}

  using StackableDB::Flush;
  using StackableDB::GetIntProperty;

  rocksdb::Status Flush(
      const rocksdb::FlushOptions& options,
      rocksdb::ColumnFamilyHandle* family) override {
    flushes.push_back({family});
    return StackableDB::Flush(options, family);
  }

  rocksdb::Status Flush(
      const rocksdb::FlushOptions& options,
      const std::vector<rocksdb::ColumnFamilyHandle*>& families) override {
    flushes.push_back(families);
    if (!flushStatus.ok()) {
      return flushStatus;
    }
    return StackableDB::Flush(options, families);
  }

  bool GetIntProperty(
      rocksdb::ColumnFamilyHandle* family,
      const rocksdb::Slice& property,
      uint64_t* value) override {
    return intPropertiesAvailable &&
        StackableDB::GetIntProperty(family, property, value);
  }

  std::vector<std::vector<rocksdb::ColumnFamilyHandle*>> flushes;
  rocksdb::Status flushStatus = rocksdb::Status::OK();
  bool intPropertiesAvailable = true;
};

void storeAccessibilityOwners(
    db::Database& database,
    rts::UnitId firstUnit,
    std::vector<std::pair<rts::Id, rts::UsetId>> facts) {
  rts::ComputedAccessibilityOwnership ownership(
      firstUnit,
      {"public", "acl:alpha"},
      rts::Usets(firstUnit + 2),
      std::move(facts));
  database.storeAccessibilityOwnership(ownership);
}

class DatabaseImplTest : public ::testing::Test {
 protected:
  static constexpr int32_t kVersion = 7;

  void SetUp() override {
    temporaryDirectory_ = std::make_unique<folly::test::TemporaryDirectory>();
    database_ = openDatabase(db::Mode::Create, startingId(), kVersion);
  }

  rts::Id startingId() const {
    return rts::Id::lowest() + 100;
  }

  std::string databasePath() const {
    return temporaryDirectory_->path().string();
  }

  std::unique_ptr<db::Database> openDatabase(
      db::Mode mode,
      rts::Id requestedStartingId,
      int32_t version,
      const ContainerOptions& containerOptions = {}) const {
    auto container = open(databasePath(), mode, containerOptions, {});
    return std::move(*container).openDatabase(requestedStartingId, 0, version);
  }

  void commitThreeFacts() {
    rts::FactSet facts(startingId());
    defineFact(facts, rts::Pid::lowest(), "alpha-one", "value-1");
    defineFact(facts, rts::Pid::lowest(), "alpha-two", "value-2");
    defineFact(facts, rts::Pid::lowest() + 1, "alpha-one", "other-type");
    database_->commit(facts);
  }

  std::unique_ptr<folly::test::TemporaryDirectory> temporaryDirectory_;
  std::unique_ptr<db::Database> database_;
};

// ============================================================
// Commit and lookup consistency
// ============================================================

TEST_F(DatabaseImplTest, CommitPublishesFactIndexesAndPredicateStats) {
  const auto firstType = rts::Pid::lowest();
  const auto secondType = firstType + 1;
  rts::FactSet facts(startingId());
  const auto firstId = defineFact(facts, firstType, "alpha", "first-value");
  const auto secondId = defineFact(facts, secondType, "alpha", "second-value");

  database_->commit(facts);

  EXPECT_EQ(database_->firstFreeId(), startingId() + 2);
  EXPECT_EQ(database_->idByKey(firstType, bytes("alpha")), firstId);
  EXPECT_EQ(database_->idByKey(secondType, bytes("alpha")), secondId);
  EXPECT_EQ(database_->typeById(secondId), secondType);
  EXPECT_EQ(database_->count(firstType), rts::Interval{1});
  EXPECT_EQ(database_->count(secondType), rts::Interval{1});

  std::optional<StoredFact> stored;
  const bool found = database_->factById(firstId, [&](auto type, auto clause) {
    stored = StoredFact{type, clause.key().str(), clause.value().str()};
  });
  ASSERT_TRUE(found);
  ASSERT_TRUE(stored.has_value());
  EXPECT_EQ(stored->type, firstType);
  EXPECT_EQ(stored->key, "alpha");
  EXPECT_EQ(stored->value, "first-value");
}

TEST_F(DatabaseImplTest, RejectsStaleBatchWithoutChangingCommittedState) {
  commitThreeFacts();
  const auto firstFreeId = database_->firstFreeId();
  rts::FactSet staleFacts(startingId());
  defineFact(staleFacts, rts::Pid::lowest(), "stale", "not-committed");

  EXPECT_THROW(database_->commit(staleFacts), std::runtime_error);

  EXPECT_EQ(database_->firstFreeId(), firstFreeId);
  EXPECT_EQ(
      database_->idByKey(rts::Pid::lowest(), bytes("stale")),
      rts::Id::invalid());
  EXPECT_EQ(database_->count(rts::Pid::lowest()), rts::Interval{2});
}

// ============================================================
// Iterator boundaries and payload demand
// ============================================================

TEST_F(DatabaseImplTest, PrefixSeekIsTypeScopedAndSupportsInclusiveRestart) {
  commitThreeFacts();
  const auto type = rts::Pid::lowest();
  auto iterator = database_->seek(type, bytes("alpha-"));

  const auto keyOnly = iterator->get(rts::FactIterator::KeyOnly);
  ASSERT_TRUE(keyOnly);
  EXPECT_EQ(keyOnly.key().str(), "alpha-one");
  EXPECT_TRUE(keyOnly.value().empty());

  const auto fullFact = iterator->get(rts::FactIterator::KeyValue);
  EXPECT_EQ(fullFact.id, keyOnly.id);
  EXPECT_EQ(fullFact.value().str(), "value-1");

  auto restarted = database_->seek(type, bytes("alpha-"), fullFact);
  EXPECT_EQ(restarted->get().id, fullFact.id);
  restarted->next();
  EXPECT_EQ(restarted->get().key().str(), "alpha-two");
  restarted->next();
  EXPECT_FALSE(restarted->get());
}

TEST_F(
    DatabaseImplTest,
    EnumerationUsesMatchingHalfOpenBoundsInBothDirections) {
  rts::FactSet facts(startingId());
  for (const auto key : {"a", "b", "c", "d"}) {
    defineFact(facts, rts::Pid::lowest(), key, "value");
  }
  database_->commit(facts);

  const std::vector<rts::Id> forwardExpected{
      startingId() + 1, startingId() + 2};
  const std::vector<rts::Id> backwardExpected{
      startingId() + 2, startingId() + 1};

  EXPECT_EQ(
      collectIds(database_->enumerate(startingId() + 1, startingId() + 3)),
      forwardExpected);
  EXPECT_EQ(
      collectIds(database_->enumerateBack(startingId() + 3, startingId() + 1)),
      backwardExpected);
  EXPECT_TRUE(
      collectIds(database_->enumerate(startingId() + 4, rts::Id::invalid()))
          .empty());
  EXPECT_TRUE(
      collectIds(database_->enumerateBack(startingId(), rts::Id::invalid()))
          .empty());
}

TEST_F(
    DatabaseImplTest,
    SectionedSeekOnlyReturnsFactsWithinTheRequestedIdRange) {
  commitThreeFacts();
  const auto type = rts::Pid::lowest();
  const std::vector<rts::Id> expected{startingId(), startingId() + 1};

  EXPECT_EQ(
      collectIds(database_->seekWithinSection(
          type, bytes("alpha-"), startingId(), database_->firstFreeId())),
      expected);
  EXPECT_TRUE(collectIds(database_->seekWithinSection(
                             type,
                             bytes("alpha-"),
                             database_->firstFreeId(),
                             database_->firstFreeId() + 10))
                  .empty());
  EXPECT_TRUE(
      collectIds(database_->seekWithinSection(
                     type, bytes("alpha-"), rts::Id::invalid(), startingId()))
          .empty());
}

// ============================================================
// Ownership statistics
// ============================================================

TEST_F(DatabaseImplTest, OwnershipStatsReportNoUnitsAndUnknownOrphanCount) {
  commitThreeFacts();

  const auto stats = database_->getOwnership()->getStats();

  EXPECT_EQ(stats.num_units, 0);
  EXPECT_EQ(stats.num_sets, 0);
  EXPECT_EQ(stats.num_orphan_facts, -1);
}

TEST_F(DatabaseImplTest, OwnershipStatsFlushTablesTogether) {
  database_->addOwnership({{bytes("a.cpp"), {}}});
  auto& container = static_cast<impl::ContainerImpl&>(database_->container());
  auto recorder = std::make_unique<StatsTestDB>(std::move(container.db));
  const auto* observed = recorder.get();
  container.db = std::move(recorder);

  const auto stats = database_->getOwnership()->getStats();

  EXPECT_GT(stats.units_size, 0);
  ASSERT_EQ(observed->flushes.size(), 1);
  const std::vector<rocksdb::ColumnFamilyHandle*> expected{
      container.family(impl::Family::ownershipUnits),
      container.family(impl::Family::ownershipUnitIds),
      container.family(impl::Family::ownershipSets),
      container.family(impl::Family::factOwners),
      container.family(impl::Family::factOwnerPages)};
  EXPECT_EQ(observed->flushes.front(), expected);
}

TEST_F(DatabaseImplTest, OwnershipStatsReportFlushFailure) {
  database_->addOwnership({{bytes("a.cpp"), {}}});
  auto& container = static_cast<impl::ContainerImpl&>(database_->container());
  auto recorder = std::make_unique<StatsTestDB>(std::move(container.db));
  recorder->flushStatus =
      rocksdb::Status::IOError("ownership stats flush failed");
  const auto* observed = recorder.get();
  container.db = std::move(recorder);

  EXPECT_THROW(database_->getOwnership()->getStats(), std::runtime_error);
  EXPECT_EQ(observed->flushes.size(), 1);
}

TEST_F(DatabaseImplTest, OwnershipStatsReportMissingKeyEstimate) {
  database_->addOwnership({{bytes("a.cpp"), {}}});
  auto& container = static_cast<impl::ContainerImpl&>(database_->container());
  auto recorder = std::make_unique<StatsTestDB>(std::move(container.db));
  recorder->intPropertiesAvailable = false;
  container.db = std::move(recorder);

  EXPECT_THROW(database_->getOwnership()->getStats(), std::runtime_error);
}

TEST_F(DatabaseImplTest, OwnershipStatsReadStoredEstimatesInReadOnlyMode) {
  database_->addOwnership({{bytes("a.cpp"), {}}});
  database_->container().flush();
  database_.reset();
  database_ = openDatabase(db::Mode::ReadOnly, startingId(), kVersion);

  const auto stats = database_->getOwnership()->getStats();

  EXPECT_GT(stats.units_size, 0);
}

TEST_F(DatabaseImplTest, ReopenedOwnerSetsKeepTheirOperation) {
  commitThreeFacts();
  database_->addOwnership({{bytes("a.cpp"), {}}, {bytes("b.cpp"), {}}});
  rts::Usets sets(2);
  const auto conjunction = rts::OwnershipExpressionBuilder(sets).allOf({0, 1});
  rts::ComputedOwnership ownership(std::move(sets), {});
  database_->storeOwnership(rts::OwnershipKind::Visibility, ownership);

  database_.reset();
  database_ = openDatabase(db::Mode::ReadWrite, startingId(), kVersion);

  auto stored = database_->getOwnership();
  rts::Uset sameConjunction(rts::SetU32::from({0, 1}), rts::And, 0);
  rts::Uset disjunction(rts::SetU32::from({0, 1}), rts::Or, 0);
  EXPECT_EQ(stored->lookupSet(&sameConjunction), conjunction);
  EXPECT_EQ(stored->lookupSet(&disjunction), rts::INVALID_USET);
}

TEST_F(DatabaseImplTest, AccessibilityMapPreservesIntervalsAndStatsOnReopen) {
  commitThreeFacts();
  constexpr rts::UnitId publicUnit = 100;
  constexpr rts::UnitId alphaUnit = publicUnit + 1;
  storeAccessibilityOwners(
      *database_,
      publicUnit,
      {{startingId(), publicUnit},
       {startingId() + 2, alphaUnit},
       {startingId() + 3, rts::INVALID_USET}});

  auto ownership = database_->getAccessibilityOwnership();
  ASSERT_NE(ownership, nullptr);
  const auto stats = ownership->getStats();
  EXPECT_EQ(stats.num_units, 2);
  EXPECT_EQ(stats.num_sets, 0);
  EXPECT_EQ(stats.num_owner_entries, 3);
  EXPECT_GT(stats.owners_size, 0);
  EXPECT_EQ(database_->getAccessibilityOwner(startingId()), publicUnit);
  EXPECT_EQ(database_->getAccessibilityOwner(startingId() + 1), publicUnit);
  EXPECT_EQ(database_->getAccessibilityOwner(startingId() + 2), alphaUnit);
  EXPECT_EQ(
      database_->getAccessibilityOwner(startingId() + 3), rts::INVALID_USET);

  ownership.reset();
  database_.reset();
  database_ = openDatabase(db::Mode::ReadOnly, startingId(), kVersion);
  EXPECT_EQ(database_->getAccessibilityOwner(startingId() + 1), publicUnit);
  EXPECT_EQ(database_->getAccessibilityOwner(startingId() + 2), alphaUnit);
}

TEST_F(
    DatabaseImplTest,
    AccessibilityRejectsMisplacedUnitsWithoutChangingTables) {
  commitThreeFacts();
  const auto reject = [&](rts::ComputedAccessibilityOwnership ownership) {
    EXPECT_THROW(
        database_->storeAccessibilityOwnership(ownership), std::runtime_error);
    EXPECT_EQ(database_->getAccessibilityOwnership(), nullptr);
    auto& container = static_cast<impl::ContainerImpl&>(database_->container());
    for (const auto* family :
         {&impl::Family::accessibilityUnits,
          &impl::Family::accessibilitySets,
          &impl::Family::accessibilityOwners}) {
      auto iter = container.read(*family);
      iter.seek_first();
      EXPECT_FALSE(iter.valid());
    }
  };

  reject({0, {}, rts::Usets(0), {}});
  reject({2, {"public"}, rts::Usets(1), {}});
  reject({0, {"public"}, rts::Usets(2), {}});
  reject({rts::INVALID_USET, {"public"}, rts::Usets(rts::INVALID_USET), {}});
}

TEST_F(DatabaseImplTest, AccessibilityRejectsIncompatibleRewrite) {
  commitThreeFacts();
  constexpr rts::UnitId publicUnit = 100;
  storeAccessibilityOwners(
      *database_,
      publicUnit,
      {{startingId(), publicUnit},
       {database_->firstFreeId(), rts::INVALID_USET}});

  EXPECT_THROW(
      storeAccessibilityOwners(
          *database_, publicUnit + 100, {{startingId(), publicUnit + 100}}),
      std::runtime_error);
  auto ownership = database_->getAccessibilityOwnership();
  ASSERT_NE(ownership, nullptr);
  EXPECT_EQ(ownership->getUnitId(bytes("public")), publicUnit);
  EXPECT_EQ(ownership->getStats().num_owner_entries, 2);
  EXPECT_EQ(database_->getAccessibilityOwner(startingId()), publicUnit);

  ownership.reset();
  database_.reset();
  database_ = openDatabase(db::Mode::ReadOnly, startingId(), kVersion);
  EXPECT_EQ(database_->getAccessibilityOwner(startingId()), publicUnit);
  EXPECT_EQ(
      database_->getAccessibilityOwnership()->getUnitId(bytes("public")),
      publicUnit);
}

TEST_F(DatabaseImplTest, AccessibilityOwnersAppendWithoutReplacingOwners) {
  commitThreeFacts();
  constexpr rts::UnitId publicUnit = 100;
  constexpr rts::UnitId alphaUnit = publicUnit + 1;
  storeAccessibilityOwners(
      *database_,
      publicUnit,
      {{startingId(), publicUnit}, {startingId() + 1, rts::INVALID_USET}});

  rts::Usets sets(publicUnit + 2);
  const auto either = sets.intern(
      std::make_unique<rts::Uset>(
          rts::SetU32::from({publicUnit, alphaUnit}), rts::Or, 0));
  rts::ComputedOwnership appended(
      std::move(sets),
      {{startingId(), rts::INVALID_USET},
       {startingId() + 2, either},
       {database_->firstFreeId(), rts::INVALID_USET}});
  database_->storeOwnership(rts::OwnershipKind::Accessibility, appended);

  const auto expectAppended = [&] {
    EXPECT_EQ(database_->getAccessibilityOwner(startingId()), publicUnit);
    EXPECT_EQ(
        database_->getAccessibilityOwner(startingId() + 1), rts::INVALID_USET);
    EXPECT_EQ(database_->getAccessibilityOwner(startingId() + 2), either);
    const auto expression =
        database_->getAccessibilityOwnership()->getUset(either);
    ASSERT_TRUE(expression.has_value());
    EXPECT_EQ(expression->op, rts::Or);
    EXPECT_EQ(
        rts::SetU32::to(expression->set),
        (std::set<uint32_t>{publicUnit, alphaUnit}));
  };
  expectAppended();

  database_.reset();
  database_ = openDatabase(db::Mode::ReadOnly, startingId(), kVersion);
  expectAppended();
}

TEST_F(DatabaseImplTest, DerivedAccessibilityOwnersHaveTheirOwnLog) {
  commitThreeFacts();
  constexpr rts::UnitId publicUnit = 100;
  constexpr rts::UnitId alphaUnit = publicUnit + 1;
  constexpr rts::UsetId first = publicUnit + 2;
  constexpr rts::UsetId second = first + 1;
  storeAccessibilityOwners(
      *database_, publicUnit, {{startingId(), publicUnit}});
  const auto pid = rts::Pid::lowest() + 1;
  const auto visibilityNextId = database_->getOwnership()->nextSetId();

  // One batch, as derivation writes it: its sets are numbered from the
  // kind's next ID, and its owners refer to them.
  const auto addDerived = [&](rts::Id fact, const std::set<uint32_t>& inputs) {
    auto& database = static_cast<impl::DatabaseImpl&>(*database_);
    const db::OwnershipKindStorage<impl::ContainerImpl> storage{
        .units = impl::Family::accessibilityUnits,
        .unitNames = nullptr,
        .sets = impl::Family::accessibilitySets,
        .owners = impl::Family::accessibilityOwners,
        .ownerPages = nullptr,
        .orphanFactsKey = std::nullopt,
        .firstUnit = database.first_accessibility_unit,
        .nextIdKey = db::AdminId::NEXT_ACCESSIBILITY_ID,
        .nextId = database.next_accessibility_id,
        .writableSets = database.accessibility_usets_,
        .derivedRaw = impl::Family::accessibilityDerivedRaw,
        .derivedCounters = database.accessibility_derived_counters};
    rts::Usets batch(storage.nextId);
    const auto owner = batch.intern(
        std::make_unique<rts::Uset>(rts::SetU32::from(inputs), rts::And, 0));
    std::map<rts::Pid, rts::DefineOwnership::PerPredicate> owners;
    owners[pid].new_ids_ = {fact};
    owners[pid].new_owners_ = {owner};
    database.addDerivedOwners(storage, batch, owners);
  };
  addDerived(startingId() + 1, {publicUnit, alphaUnit});

  // Reopening for writing continues the log.
  database_.reset();
  database_ = openDatabase(db::Mode::ReadWrite, startingId(), kVersion);
  addDerived(startingId() + 2, {alphaUnit, first});

  std::vector<std::pair<std::vector<rts::Id>, std::vector<rts::UsetId>>>
      entries;
  auto log = database_->getDerivedFactOwnershipIterator(
      rts::OwnershipKind::Accessibility, pid);
  while (const auto entry = log->get()) {
    entries.emplace_back(
        std::vector<rts::Id>(entry->ids.begin(), entry->ids.end()),
        std::vector<rts::UsetId>(entry->owners.begin(), entry->owners.end()));
  }
  const decltype(entries) expected = {
      {{startingId() + 1}, {first}}, {{startingId() + 2}, {second}}};
  EXPECT_EQ(entries, expected);

  auto ownership = database_->getAccessibilityOwnership();
  EXPECT_EQ(ownership->nextSetId(), second + 1);
  for (const auto& [owner, members] :
       {std::pair{first, std::set<uint32_t>{publicUnit, alphaUnit}},
        std::pair{second, std::set<uint32_t>{alphaUnit, first}}}) {
    const auto expression = ownership->getUset(owner);
    ASSERT_TRUE(expression.has_value());
    EXPECT_EQ(expression->op, rts::And);
    EXPECT_EQ(rts::SetU32::to(expression->set), members);
  }

  EXPECT_FALSE(
      database_
          ->getDerivedFactOwnershipIterator(rts::OwnershipKind::Visibility, pid)
          ->get()
          .has_value());
  EXPECT_EQ(database_->getOwnership()->nextSetId(), visibilityNextId);
}

TEST_F(DatabaseImplTest, AccessibilityPublicUnitZeroSurvivesReopen) {
  commitThreeFacts();
  storeAccessibilityOwners(
      *database_,
      0,
      {{startingId(), 0}, {database_->firstFreeId(), rts::INVALID_USET}});
  auto ownership = database_->getAccessibilityOwnership();
  ASSERT_NE(ownership, nullptr);
  EXPECT_EQ(ownership->nextSetId(), 2);
  EXPECT_EQ(database_->getAccessibilityOwner(startingId() + 1), 0);

  ownership.reset();
  database_.reset();
  database_ = openDatabase(db::Mode::ReadOnly, startingId(), kVersion);
  ownership = database_->getAccessibilityOwnership();
  ASSERT_NE(ownership, nullptr);
  EXPECT_EQ(ownership->getUnitId(bytes("public")), 0);
  EXPECT_EQ(database_->getAccessibilityOwner(startingId() + 1), 0);
  EXPECT_EQ(
      database_->getAccessibilityOwner(database_->firstFreeId()),
      rts::INVALID_USET);
}

TEST_F(DatabaseImplTest, AccessibilityRejectsPersistedIdsWiderThanUsetId) {
  commitThreeFacts();
  storeAccessibilityOwners(*database_, 0, {{startingId(), 0}});
  auto& container = static_cast<impl::ContainerImpl&>(database_->container());
  {
    auto writer = container.write();
    rts::EncodedNat key(startingId().toWord());
    rts::EncodedNat value(uint64_t{1} << 32);
    writer.put(
        impl::Family::accessibilityOwners, key.byteRange(), value.byteRange());
    writer.put(impl::Family::factOwners, key.byteRange(), value.byteRange());
    writer.commit();
  }

  EXPECT_THROW(
      database_->getAccessibilityOwner(startingId()), std::runtime_error);
  EXPECT_THROW(database_->getOwner(startingId()), std::runtime_error);
}

TEST_F(DatabaseImplTest, OpenRejectsMisSizedNextAccessibilityId) {
  auto& container = static_cast<impl::ContainerImpl&>(database_->container());
  {
    binary::Output key;
    key.fixed(db::AdminId::NEXT_ACCESSIBILITY_ID);
    binary::Output value;
    value.fixed(uint64_t{1});
    auto writer = container.write();
    writer.put(impl::Family::admin, key.bytes(), value.bytes());
    writer.commit();
  }
  database_.reset();

  try {
    openDatabase(db::Mode::ReadOnly, startingId(), kVersion);
    ADD_FAILURE() << "opened a database with a mis-sized NEXT_ACCESSIBILITY_ID";
  } catch (const std::exception& e) {
    const std::string_view message = e.what();
    EXPECT_NE(message.find("NEXT_ACCESSIBILITY_ID"), std::string_view::npos);
  }
}

TEST_F(DatabaseImplTest, AccessibilityLookupCacheReadsStoredFactOwners) {
  commitThreeFacts();
  constexpr rts::UnitId publicUnit = 100;
  constexpr rts::UnitId alphaUnit = publicUnit + 1;
  storeAccessibilityOwners(
      *database_,
      publicUnit,
      {{startingId(), publicUnit},
       {startingId() + 1, alphaUnit},
       {startingId() + 2, rts::INVALID_USET}});

  ASSERT_EQ(database_->firstFreeId(), startingId() + 3);
  auto ownership = database_->getAccessibilityOwnership();
  ASSERT_NE(ownership, nullptr);
  EXPECT_EQ(ownership->getStats().num_owner_entries, 3);
  EXPECT_EQ(ownership->nextSetId(), publicUnit + 2);

  {
    rts::LookupCache::Options options;
    options.capacity = 1024;
    options.shards = 1;
    auto stats = std::make_shared<rts::LookupCache::Stats>();
    rts::LookupCache cache(options, stats);
    auto lookup = cache.anchor(database_.get());

    EXPECT_EQ(lookup.getAccessibilityOwner(startingId()), publicUnit);
    EXPECT_EQ(lookup.getAccessibilityOwner(startingId() + 1), alphaUnit);
    EXPECT_EQ(
        lookup.getAccessibilityOwner(startingId() + 2), rts::INVALID_USET);
    EXPECT_EQ(lookup.getOwner(startingId()), rts::INVALID_USET);

    rts::Section bounded(&lookup, startingId(), startingId() + 1);
    EXPECT_EQ(bounded.getAccessibilityOwner(startingId()), publicUnit);
    EXPECT_EQ(
        bounded.getAccessibilityOwner(startingId() + 1), rts::INVALID_USET);
  }

  database_.reset();
  database_ = openDatabase(db::Mode::ReadOnly, startingId(), kVersion);
  EXPECT_EQ(database_->getAccessibilityOwner(startingId()), publicUnit);
  EXPECT_EQ(database_->getAccessibilityOwner(startingId() + 1), alphaUnit);
}

TEST_F(DatabaseImplTest, StackedAccessibilityLookupUsesTheTopmostStoredOwner) {
  commitThreeFacts();
  constexpr rts::UnitId basePublic = 100;
  constexpr rts::UnitId layerPublic = 200;
  storeAccessibilityOwners(
      *database_,
      basePublic,
      {{startingId(), basePublic},
       {startingId() + 1, basePublic + 1},
       {startingId() + 2, rts::INVALID_USET}});

  folly::test::TemporaryDirectory layerDirectory;
  auto container = open(
      layerDirectory.path().string(), db::Mode::Create, ContainerOptions{}, {});
  auto layer =
      std::move(*container).openDatabase(database_->firstFreeId(), 0, kVersion);
  rts::FactSet facts(layer->startingId());
  const auto newFact = defineFact(facts, rts::Pid::lowest(), "layer", "value");
  layer->commit(facts);
  storeAccessibilityOwners(
      *layer,
      layerPublic,
      {{startingId() + 1, layerPublic},
       {startingId() + 2, rts::INVALID_USET},
       {newFact, layerPublic + 1}});

  rts::Stacked<rts::Lookup> lookup(database_.get(), layer.get());
  EXPECT_EQ(lookup.getAccessibilityOwner(startingId()), basePublic);
  EXPECT_EQ(lookup.getAccessibilityOwner(startingId() + 1), layerPublic);
  EXPECT_EQ(lookup.getAccessibilityOwner(startingId() + 2), rts::INVALID_USET);
  EXPECT_EQ(lookup.getAccessibilityOwner(newFact), layerPublic + 1);
  EXPECT_EQ(
      lookup.getAccessibilityOwner(layer->firstFreeId()), rts::INVALID_USET);
  EXPECT_EQ(lookup.getOwner(startingId() + 1), rts::INVALID_USET);
}

// ============================================================
// Persistent database metadata
// ============================================================

TEST_F(DatabaseImplTest, ReopenUsesPersistedBoundsFactsAndStats) {
  commitThreeFacts();
  database_.reset();

  database_ = openDatabase(
      db::Mode::ReadOnly, startingId() + 500, DatabaseImplTest::kVersion);

  EXPECT_EQ(database_->startingId(), startingId());
  EXPECT_EQ(database_->firstFreeId(), startingId() + 3);
  EXPECT_EQ(
      database_->idByKey(rts::Pid::lowest(), bytes("alpha-two")),
      startingId() + 1);
  EXPECT_EQ(database_->count(rts::Pid::lowest()), rts::Interval{2});
  EXPECT_EQ(database_->count(rts::Pid::lowest() + 1), rts::Interval{1});
}

TEST_F(DatabaseImplTest, TunedContainerOptionsWriteAndReopen) {
  ContainerOptions tuned;
  tuned.cache_index_and_filter_blocks = true;
  tuned.background_jobs = 4;
  database_.reset();
  temporaryDirectory_ = std::make_unique<folly::test::TemporaryDirectory>();
  database_ = openDatabase(db::Mode::Create, startingId(), kVersion, tuned);

  commitThreeFacts();
  database_.reset();
  database_ = openDatabase(db::Mode::ReadOnly, startingId(), kVersion, tuned);

  EXPECT_EQ(database_->firstFreeId(), startingId() + 3);
  EXPECT_EQ(
      database_->idByKey(rts::Pid::lowest(), bytes("alpha-two")),
      startingId() + 1);
  EXPECT_EQ(
      database_->idByKey(rts::Pid::lowest(), bytes("missing")),
      rts::Id::invalid());
}

TEST_F(DatabaseImplTest, ReopenRejectsUnexpectedDatabaseVersion) {
  database_.reset();

  EXPECT_THROW(
      openDatabase(db::Mode::ReadOnly, startingId(), kVersion + 1),
      std::runtime_error);
}

} // namespace

} // namespace facebook::glean::rocks

int main(int argc, char** argv) {
  ::testing::InitGoogleTest(&argc, argv);
  const folly::Init init(&argc, &argv, folly::InitOptions().useGFlags(false));
  const int result = RUN_ALL_TESTS();
  std::fflush(stdout);
  std::fflush(stderr);
  // RocksDB's monitoring/logging threads can block indefinitely at static
  // destruction time in the test environment.
  std::_Exit(result);
}
