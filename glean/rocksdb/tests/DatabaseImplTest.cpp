/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <gtest/gtest.h>

#include <cstdint>
#include <memory>
#include <optional>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

#include <folly/Range.h>
#include <folly/testing/TestUtil.h>

#include "glean/rocksdb/rocksdb.h"

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
      int32_t version) const {
    auto container = open(databasePath(), mode, false, {});
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

TEST_F(DatabaseImplTest, ReopenRejectsUnexpectedDatabaseVersion) {
  database_.reset();

  EXPECT_THROW(
      openDatabase(db::Mode::ReadOnly, startingId(), kVersion + 1),
      std::runtime_error);
}

} // namespace

} // namespace facebook::glean::rocks
