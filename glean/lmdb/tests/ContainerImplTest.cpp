/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <gmock/gmock.h>
#include <gtest/gtest.h>

#include <filesystem>
#include <optional>
#include <string>
#include <utility>
#include <vector>

#include <folly/Range.h>
#include <folly/testing/TestUtil.h>

#include "glean/lmdb/container-impl.h"
#include "glean/lmdb/database-impl.h"
#include "glean/rts/factset.h"

namespace facebook::glean::lmdb::impl {

namespace {

folly::ByteRange bytes(folly::StringPiece s) {
  return folly::ByteRange(
      reinterpret_cast<const unsigned char*>(s.data()), s.size());
}

std::string toString(folly::ByteRange r) {
  return std::string(reinterpret_cast<const char*>(r.data()), r.size());
}

void writeMeta(
    ContainerImpl& c,
    folly::StringPiece key,
    folly::StringPiece value) {
  c.writeData(bytes(key), bytes(value));
}

std::optional<std::string> readMeta(ContainerImpl& c, folly::StringPiece key) {
  std::optional<std::string> out;
  c.readData(bytes(key), [&](folly::ByteRange v) { out = toString(v); });
  return out;
}

} // namespace

// ============================================================
// Family registry: static definitions live in container-impl.cpp
// ============================================================

TEST(FamilyRegistryTest, LookupByNameReturnsMatchingFamily) {
  EXPECT_EQ(Family::family("meta"), &Family::meta);
  EXPECT_EQ(Family::family("ownershipSets"), &Family::ownershipSets);
}

TEST(FamilyRegistryTest, LookupByUnknownNameReturnsNull) {
  EXPECT_EQ(Family::family("no-such-family"), nullptr);
}

TEST(FamilyRegistryTest, LookupByIndexOutOfRangeReturnsNull) {
  EXPECT_EQ(Family::family(Family::count()), nullptr);
}

TEST(FamilyRegistryTest, IndexMatchesRegistrationPosition) {
  ASSERT_GT(Family::count(), 0u);
  for (size_t i = 0; i < Family::count(); ++i) {
    const auto* family = Family::family(i);
    ASSERT_NE(family, nullptr);
    EXPECT_EQ(family->index, i);
  }
}

// ============================================================
// Container: open/read/write against a real on-disk LMDB env
// ============================================================

class ContainerImplTest : public ::testing::Test {
 protected:
  void SetUp() override {
    tmpDir_ = std::make_unique<folly::test::TemporaryDirectory>();
    container_ = std::make_unique<ContainerImpl>(dbPath(), Mode::Create);
  }

  std::string dbPath() const {
    return tmpDir_->path().string();
  }

  std::unique_ptr<folly::test::TemporaryDirectory> tmpDir_;
  std::unique_ptr<ContainerImpl> container_;
};

TEST_F(ContainerImplTest, WriteDataThenReadDataReturnsStoredValue) {
  writeMeta(*container_, "greeting", "hello");

  const auto v = readMeta(*container_, "greeting");
  ASSERT_TRUE(v.has_value());
  EXPECT_EQ(*v, "hello");
}

TEST_F(ContainerImplTest, ReadDataForMissingKeyReturnsFalse) {
  bool callbackInvoked = false;
  const bool found = container_->readData(
      bytes("absent"), [&](folly::ByteRange) { callbackInvoked = true; });

  EXPECT_FALSE(found);
  EXPECT_FALSE(callbackInvoked);
}

TEST_F(ContainerImplTest, WriteDataOverwritesPreviousValueForSameKey) {
  writeMeta(*container_, "key", "first");
  writeMeta(*container_, "key", "second");

  const auto v = readMeta(*container_, "key");
  ASSERT_TRUE(v.has_value());
  EXPECT_EQ(*v, "second");
}

// The `keys` family is opened with MDB_DUPSORT; the constructor must apply
// each Family's flags so a single key can hold multiple distinct values.
TEST_F(ContainerImplTest, KeysFamilySupportsDuplicateValuesPerKey) {
  {
    auto writer = container_->write();
    writer.put(Family::keys, bytes("dup"), bytes("v1"));
    writer.put(Family::keys, bytes("dup"), bytes("v2"));
    writer.commit();
  }

  std::vector<std::string> values;
  auto it = container_->read(Family::keys);
  if (it.seek_key(bytes("dup"))) {
    do {
      values.push_back(toString(it.value()));
    } while (it.next_dup());
  }

  EXPECT_THAT(values, ::testing::UnorderedElementsAre("v1", "v2"));
}

TEST_F(ContainerImplTest, DataPersistsAfterReopeningReadOnly) {
  writeMeta(*container_, "persisted", "value");
  container_.reset(); // flush + close on destruction

  ContainerImpl reopened(dbPath(), Mode::ReadOnly);
  const auto v = readMeta(reopened, "persisted");
  ASSERT_TRUE(v.has_value());
  EXPECT_EQ(*v, "value");
}

TEST_F(ContainerImplTest, TableStatsReportInvalidHandle) {
  DatabaseImpl database(std::move(*container_), Id::lowest(), 0, 7);
  const DatabaseImpl::TableGroup tables{&Family::ownershipUnits};
  mdb_dbi_close(
      database.container_.db.get(),
      database.container_.family(Family::ownershipUnits));

  EXPECT_THROW(database.tableStats(tables), std::runtime_error);
}

TEST_F(ContainerImplTest, AccessibilityUnavailableOnLegacyReadOnlyDatabase) {
  writeMeta(*container_, "persisted", "value");
  {
    DatabaseImpl database(std::move(*container_), Id::lowest(), 0, 7);
    auto txn = database.container_.txn_write();
    for (const auto* family :
         {&Family::accessibilityUnits,
          &Family::accessibilitySets,
          &Family::accessibilityOwners,
          &Family::accessibilityDerivedRaw}) {
      check(mdb_drop(txn.ptr(), database.container_.family(*family), 1));
    }
    txn.commit();
  }
  container_.reset();

  DatabaseImpl reopened(
      ContainerImpl(dbPath(), Mode::ReadOnly), Id::lowest(), 0, 7);
  EXPECT_EQ(readMeta(reopened.container_, "persisted"), "value");
  EXPECT_EQ(reopened.getAccessibilityOwnership(), nullptr);
  EXPECT_EQ(reopened.getAccessibilityOwner(Id::lowest()), rts::INVALID_USET);
  for (const auto* family :
       {&Family::accessibilityUnits,
        &Family::accessibilitySets,
        &Family::accessibilityOwners,
        &Family::accessibilityDerivedRaw}) {
    EXPECT_THROW(reopened.container_.read(*family), std::runtime_error);
  }
}

TEST_F(ContainerImplTest, ReadOnlyOpenLoadsStoredPredicateStats) {
  {
    DatabaseImpl database(std::move(*container_), Id::lowest(), 0, 7);
    rts::FactSet facts(Id::lowest());
    const std::string clause = "keyvalue";
    facts.define(Pid::lowest(), rts::Fact::Clause::from(bytes(clause), 3));
    database.commit(facts);
  }
  container_.reset();

  DatabaseImpl reopened(
      ContainerImpl(dbPath(), Mode::ReadOnly), Id::lowest(), 0, 7);
  EXPECT_EQ(reopened.count(Pid::lowest()), rts::Interval{1});
}

class CorruptStatsTest : public ContainerImplTest {
 protected:
  void expectReadOnlyOpenRejectsStatsOfSize(size_t size) {
    {
      DatabaseImpl database(std::move(*container_), Id::lowest(), 0, 7);
      binary::Output key;
      key.fixed(Pid::lowest());
      const std::string value(size, 'A');
      auto writer = database.container_.write();
      writer.put(Family::stats, key.bytes(), bytes(value));
      writer.commit();
    }
    container_.reset();

    EXPECT_THROW(
        DatabaseImpl(
            ContainerImpl(dbPath(), Mode::ReadOnly), Id::lowest(), 0, 7),
        std::runtime_error);
  }
};

TEST_F(CorruptStatsTest, ReadOnlyOpenRejectsOversizedPredicateStats) {
  expectReadOnlyOpenRejectsStatsOfSize(sizeof(rts::MemoryStats) * 4);
}

TEST_F(CorruptStatsTest, ReadOnlyOpenRejectsTruncatedPredicateStats) {
  expectReadOnlyOpenRejectsStatsOfSize(sizeof(rts::MemoryStats) / 2);
}

TEST_F(ContainerImplTest, GetUnitIdRejectsMissizedStoredUnitId) {
  DatabaseImpl database(std::move(*container_), Id::lowest(), 0, 7);
  auto writer = database.container_.write();
  writer.put(Family::ownershipUnits, bytes("short.cpp"), bytes("ab"));
  writer.put(Family::ownershipUnits, bytes("long.cpp"), bytes("abcdef"));
  writer.commit();

  EXPECT_THROW(database.getUnitId(bytes("short.cpp")), std::runtime_error);
  EXPECT_THROW(database.getUnitId(bytes("long.cpp")), std::runtime_error);
}

TEST_F(ContainerImplTest, GetUnitsByPrefixReturnsStoredUnitIds) {
  DatabaseImpl database(std::move(*container_), Id::lowest(), 0, 7);
  const uint32_t unit = 0x01020304;
  auto writer = database.container_.write();
  writer.put(Family::ownershipUnits, bytes("a.cpp"), db::bytesOf(unit));
  writer.commit();

  const std::vector<std::pair<std::string, uint32_t>> expected{{"a.cpp", unit}};
  EXPECT_EQ(database.getUnitsByPrefix(bytes("a")), expected);
}

TEST_F(ContainerImplTest, GetUnitsByPrefixRejectsMissizedStoredUnitId) {
  DatabaseImpl database(std::move(*container_), Id::lowest(), 0, 7);
  auto writer = database.container_.write();
  writer.put(Family::ownershipUnits, bytes("short.cpp"), bytes("ab"));
  writer.put(Family::ownershipUnits, bytes("long.cpp"), bytes("abcdef"));
  writer.commit();

  EXPECT_THROW(database.getUnitsByPrefix(bytes("short")), std::runtime_error);
  EXPECT_THROW(database.getUnitsByPrefix(bytes("long")), std::runtime_error);
}

TEST_F(ContainerImplTest, FactByIdReturnsStoredClause) {
  DatabaseImpl database(std::move(*container_), Id::lowest(), 0, 7);
  rts::FactSet facts(Id::lowest());
  const std::string clause = "keyvalue";
  const auto id =
      facts.define(Pid::lowest(), rts::Fact::Clause::from(bytes(clause), 3));
  database.commit(facts);

  std::optional<std::pair<std::string, std::string>> stored;
  const bool found = database.factById(id, [&](Pid, rts::Fact::Clause c) {
    stored = {toString(c.key()), toString(c.value())};
  });

  EXPECT_TRUE(found);
  EXPECT_EQ(stored, std::make_pair(std::string("key"), std::string("value")));
}

TEST_F(ContainerImplTest, FactByIdReturnsFactWithEmptyValue) {
  DatabaseImpl database(std::move(*container_), Id::lowest(), 0, 7);
  rts::FactSet facts(Id::lowest());
  const std::string clause = "just-a-key";
  const auto id = facts.define(
      Pid::lowest(), rts::Fact::Clause::from(bytes(clause), clause.size()));
  database.commit(facts);

  std::optional<std::pair<std::string, std::string>> stored;
  const bool found = database.factById(id, [&](Pid, rts::Fact::Clause c) {
    stored = {toString(c.key()), toString(c.value())};
  });

  EXPECT_TRUE(found);
  EXPECT_EQ(stored, std::make_pair(clause, std::string()));
}

TEST_F(ContainerImplTest, FactByIdRejectsKeySizeOneBeyondStoredClause) {
  DatabaseImpl database(std::move(*container_), Id::lowest(), 0, 7);
  rts::FactSet facts(Id::lowest());
  const std::string clause = "keyvalue";
  const auto id =
      facts.define(Pid::lowest(), rts::Fact::Clause::from(bytes(clause), 3));
  database.commit(facts);
  binary::Output key;
  key.nat(id.toWord());
  binary::Output value;
  value.packed(Pid::lowest());
  value.packed(uint32_t{9});
  value.put(bytes(clause));
  auto writer = database.container_.write();
  writer.put(Family::entities, key.bytes(), value.bytes());
  writer.commit();

  EXPECT_THROW(
      database.factById(id, [](Pid, rts::Fact::Clause) {}), std::runtime_error);
}

TEST_F(ContainerImplTest, FactByIdRejectsKeySizeBeyondStoredClause) {
  DatabaseImpl database(std::move(*container_), Id::lowest(), 0, 7);
  rts::FactSet facts(Id::lowest());
  const std::string clause = "keyvalue";
  const auto id =
      facts.define(Pid::lowest(), rts::Fact::Clause::from(bytes(clause), 3));
  database.commit(facts);
  binary::Output key;
  key.nat(id.toWord());
  binary::Output value;
  value.packed(Pid::lowest());
  value.packed(uint32_t{1} << 20);
  value.put(bytes(std::string(40, 'k')));
  auto writer = database.container_.write();
  writer.put(Family::entities, key.bytes(), value.bytes());
  writer.commit();

  EXPECT_THROW(
      database.factById(id, [](Pid, rts::Fact::Clause) {}), std::runtime_error);
}

// Mirrors the shutdown flow: flush a still-writable container, then back it
// up. The backup must be a self-contained, readable copy.
TEST_F(ContainerImplTest, FlushThenBackupProducesReadableCopy) {
  writeMeta(*container_, "backed-up", "value");
  container_->flush();

  const std::string backupPath = tmpDir_->path().string() + "/backup";
  std::filesystem::create_directory(backupPath);
  container_->backup(backupPath);
  container_.reset();

  ContainerImpl restored(backupPath, Mode::ReadOnly);
  const auto v = readMeta(restored, "backed-up");
  ASSERT_TRUE(v.has_value());
  EXPECT_EQ(*v, "value");
}

// Kept families (like `meta`) must survive optimize(), which only clears
// families flagged keep = false.
TEST_F(ContainerImplTest, OptimizeKeepsDurableFamilyData) {
  writeMeta(*container_, "durable", "value");

  container_->optimize(/*compact=*/false);

  const auto v = readMeta(*container_, "durable");
  ASSERT_TRUE(v.has_value());
  EXPECT_EQ(*v, "value");
}

TEST_F(ContainerImplTest, WriteDataAfterCloseThrows) {
  container_->close();

  EXPECT_THROW(writeMeta(*container_, "k", "v"), std::runtime_error);
}

TEST_F(ContainerImplTest, FlushAfterCloseThrows) {
  container_->close();

  EXPECT_THROW(container_->flush(), std::runtime_error);
}

} // namespace facebook::glean::lmdb::impl
