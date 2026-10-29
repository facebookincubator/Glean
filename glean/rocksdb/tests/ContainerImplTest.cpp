/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <gmock/gmock.h>
#include <gtest/gtest.h>

#include "glean/rocksdb/container-impl.h"

#include <cstdio>
#include <cstdlib>
#include <filesystem>
#include <limits>
#include <optional>
#include <string>
#include <vector>

#include <fmt/core.h>
#include <folly/Range.h>
#include <folly/init/Init.h>
#include <folly/testing/TestUtil.h>
#include <rocksdb/db.h>
#include <rocksdb/table.h>
#include <rocksdb/table_properties.h>

namespace facebook::glean::rocks::impl {

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

std::vector<std::string> readFamilyValues(
    ContainerImpl& c,
    const Family& family) {
  std::vector<std::string> values;
  auto it = c.read(family);
  for (it.seek_first(); it.valid(); it.next()) {
    values.push_back(toString(it.value()));
  }
  return values;
}

std::vector<std::string> compressionNames(
    ContainerImpl& c,
    const Family& family) {
  rocksdb::TablePropertiesCollection props;
  check(c.db->GetPropertiesOfAllTables(c.family(family), &props));
  std::vector<std::string> names;
  for (const auto& [file, p] : props) {
    names.push_back(
        rocksdb::ParseCompressionNameForDisplay(p->compression_name));
  }
  return names;
}

std::string bigEndianKey(uint64_t n) {
  std::string key(sizeof(n), '\0');
  for (size_t i = 0; i < sizeof(n); ++i) {
    key[sizeof(n) - 1 - i] = static_cast<char>(n >> (8 * i));
  }
  return key;
}

// Writes rows with increasing keys to entities in several flushed batches, so
// that, like a real Glean DB, entities ends up with non-overlapping files.
void writeEntitiesInBatches(ContainerImpl& c) {
  constexpr uint64_t kBatches = 3;
  constexpr uint64_t kRowsPerBatch = 2000;
  for (uint64_t batch = 0; batch < kBatches; ++batch) {
    auto writer = c.write();
    for (uint64_t i = 0; i < kRowsPerBatch; ++i) {
      const auto id = batch * kRowsPerBatch + i;
      const auto value = fmt::format("fact {} of predicate {}", id, id % 7);
      writer.put(Family::entities, bytes(bigEndianKey(id)), bytes(value));
    }
    writer.commit();
    c.flush();
  }
}

std::vector<std::string> filterPolicyNames(
    ContainerImpl& c,
    const Family& family) {
  rocksdb::TablePropertiesCollection props;
  check(c.db->GetPropertiesOfAllTables(c.family(family), &props));
  std::vector<std::string> names;
  for (const auto& [file, p] : props) {
    names.push_back(p->filter_policy_name);
  }
  return names;
}

void removeBatchDescriptorsFamily(const std::string& path) {
  rocksdb::Options options;
  std::vector<std::string> names;
  check(rocksdb::DB::ListColumnFamilies(options, path, &names));

  std::vector<rocksdb::ColumnFamilyDescriptor> descriptors;
  descriptors.reserve(names.size());
  for (const auto& name : names) {
    descriptors.emplace_back(name, rocksdb::ColumnFamilyOptions(options));
  }

  std::vector<rocksdb::ColumnFamilyHandle*> handles;
  std::unique_ptr<rocksdb::DB> db;
  check(rocksdb::DB::Open(options, path, descriptors, &handles, &db));

  for (auto* handle : handles) {
    if (handle->GetName() == Family::batchDescriptors.name) {
      check(db->DropColumnFamily(handle));
    }
    check(db->DestroyColumnFamilyHandle(handle));
  }
}

} // namespace

// ============================================================
// Container: open/read/write against a real on-disk RocksDB
// ============================================================

class ContainerImplTest : public ::testing::Test {
 protected:
  void SetUp() override {
    tmpDir_ = std::make_unique<folly::test::TemporaryDirectory>();
    container_ = std::make_unique<ContainerImpl>(
        dbPath(), Mode::Create, ContainerOptions{}, folly::none);
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

TEST_F(ContainerImplTest, WriterPublishesBufferedWritesOnlyOnCommit) {
  auto writer = container_->write();
  writer.put(Family::meta, bytes("buffered"), bytes("value"));

  EXPECT_EQ(readMeta(*container_, "buffered"), std::nullopt);
  writer.commit();
  EXPECT_EQ(
      readMeta(*container_, "buffered"), std::optional<std::string>{"value"});
}

TEST_F(ContainerImplTest, DataPersistsAfterReopeningReadOnly) {
  writeMeta(*container_, "persisted", "value");
  container_.reset();

  ContainerImpl reopened(
      dbPath(), Mode::ReadOnly, ContainerOptions{}, folly::none);
  const auto v = readMeta(reopened, "persisted");
  ASSERT_TRUE(v.has_value());
  EXPECT_EQ(*v, "value");
}

TEST_F(ContainerImplTest, CreateRefusesToOverwriteExistingDatabase) {
  writeMeta(*container_, "preserved", "value");
  container_.reset();

  EXPECT_THROW(
      std::make_unique<ContainerImpl>(
          dbPath(), Mode::Create, ContainerOptions{}, folly::none),
      std::runtime_error);

  ContainerImpl reopened(
      dbPath(), Mode::ReadOnly, ContainerOptions{}, folly::none);
  EXPECT_EQ(
      readMeta(reopened, "preserved"), std::optional<std::string>{"value"});
}

TEST_F(ContainerImplTest, OptimizeDropsTemporaryFamilyDataButKeepsMetadata) {
  {
    auto writer = container_->write();
    writer.put(Family::ownershipRaw, bytes("temporary"), bytes("raw"));
    writer.put(Family::meta, bytes("durable"), bytes("metadata"));
    writer.commit();
  }

  container_->optimize(/*compact=*/false);

  EXPECT_THAT(
      readFamilyValues(*container_, Family::ownershipRaw),
      ::testing::IsEmpty());
  const auto v = readMeta(*container_, "durable");
  ASSERT_TRUE(v.has_value());
  EXPECT_EQ(*v, "metadata");
}

TEST_F(ContainerImplTest, CompactionKeepsLz4ByDefault) {
  writeEntitiesInBatches(*container_);

  container_->optimize(/*compact=*/true);

  EXPECT_THAT(
      compressionNames(*container_, Family::entities),
      ::testing::AllOf(
          ::testing::Not(::testing::IsEmpty()),
          ::testing::Each(std::string("LZ4"))));
}

TEST_F(ContainerImplTest, ZstdTrainingBytesAreCappedAtUint32Max) {
  constexpr uint32_t kDictBytesWhose100xOverflowsUint32 = 64 << 20;
  container_.reset();
  tmpDir_ = std::make_unique<folly::test::TemporaryDirectory>();

  const ContainerImpl container(
      dbPath(),
      Mode::Create,
      ContainerOptions{
          .bottommost_zstd_dict_bytes = kDictBytesWhose100xOverflowsUint32},
      folly::none);

  EXPECT_EQ(
      container.options.bottommost_compression_opts.zstd_max_train_bytes,
      std::numeric_limits<uint32_t>::max());
}

TEST_F(ContainerImplTest, BottommostZstdRecompressesNonOverlappingFiles) {
  container_.reset();
  tmpDir_ = std::make_unique<folly::test::TemporaryDirectory>();
  container_ = std::make_unique<ContainerImpl>(
      dbPath(),
      Mode::Create,
      ContainerOptions{.bottommost_zstd_dict_bytes = 4096},
      folly::none);
  writeEntitiesInBatches(*container_);
  ASSERT_THAT(
      compressionNames(*container_, Family::entities),
      ::testing::Each(std::string("LZ4")));

  container_->optimize(/*compact=*/true);

  EXPECT_THAT(
      compressionNames(*container_, Family::entities),
      ::testing::AllOf(
          ::testing::Not(::testing::IsEmpty()),
          ::testing::Each(std::string("ZSTD"))));

  container_.reset();
  ContainerImpl reopened(
      dbPath(), Mode::ReadOnly, ContainerOptions{}, folly::none);
  std::optional<std::string> fact;
  reopened.get(
      Family::entities, bytes(bigEndianKey(4321)), [&](folly::ByteRange v) {
        fact = toString(v);
      });
  EXPECT_EQ(fact, std::optional<std::string>{"fact 4321 of predicate 2"});
}

TEST_F(ContainerImplTest, EntitiesHaveNoBloomFilterButKeysDo) {
  {
    auto writer = container_->write();
    writer.put(Family::entities, bytes("fact-id"), bytes("fact"));
    writer.put(Family::keys, bytes("fact-key"), bytes("fact-id"));
    writer.commit();
  }
  container_->optimize(/*compact=*/false);

  EXPECT_EQ(
      filterPolicyNames(*container_, Family::entities),
      std::vector<std::string>{""});
  EXPECT_THAT(
      filterPolicyNames(*container_, Family::keys),
      ::testing::ElementsAre(::testing::Not(::testing::IsEmpty())));

  container_.reset();
  ContainerImpl reopened(
      dbPath(), Mode::ReadOnly, ContainerOptions{}, folly::none);
  std::optional<std::string> fact;
  reopened.get(Family::entities, bytes("fact-id"), [&](folly::ByteRange v) {
    fact = toString(v);
  });
  EXPECT_EQ(fact, std::optional<std::string>{"fact"});
}

TEST(FamilyOptionsTest, EntitiesLeaveNonBlockBasedTableFactoryUnchanged) {
  rocksdb::ColumnFamilyOptions opts;
  opts.table_factory.reset(rocksdb::NewPlainTableFactory());

  Family::entities.options(opts);

  EXPECT_STREQ(opts.table_factory->Name(), "PlainTable");
}

TEST_F(ContainerImplTest, WritableBackupCapturesUnflushedData) {
  writeMeta(*container_, "backed-up", "value");

  const std::string backupPath = tmpDir_->path().string() + "/backup";
  std::filesystem::create_directory(backupPath);
  container_->backup(backupPath);
  container_.reset();

  const std::string restoredPath = tmpDir_->path().string() + "/restored";
  restore(restoredPath, backupPath);

  ContainerImpl restored(
      restoredPath, Mode::ReadOnly, ContainerOptions{}, folly::none);
  const auto v = readMeta(restored, "backed-up");
  ASSERT_TRUE(v.has_value());
  EXPECT_EQ(*v, "value");
}

TEST_F(ContainerImplTest, ReadOnlyCheckpointProducesOpenableSnapshot) {
  writeMeta(*container_, "checkpointed", "value");
  container_->flush();
  container_.reset();

  ContainerImpl source(
      dbPath(), Mode::ReadOnly, ContainerOptions{}, folly::none);
  const std::string checkpointPath = tmpDir_->path().string() + "/checkpoint";
  source.checkpoint(checkpointPath);

  ContainerImpl checkpoint(
      checkpointPath, Mode::ReadOnly, ContainerOptions{}, folly::none);
  const auto v = readMeta(checkpoint, "checkpointed");
  ASSERT_TRUE(v.has_value());
  EXPECT_EQ(*v, "value");
}

TEST_F(ContainerImplTest, ReadOnlyOpenSucceedsWhenOldDbLacksBatchDescriptors) {
  writeMeta(*container_, "old-db", "value");
  container_.reset();
  removeBatchDescriptorsFamily(dbPath());

  ContainerImpl reopened(
      dbPath(), Mode::ReadOnly, ContainerOptions{}, folly::none);
  const auto v = readMeta(reopened, "old-db");
  ASSERT_TRUE(v.has_value());
  EXPECT_EQ(*v, "value");
  EXPECT_EQ(reopened.family(Family::batchDescriptors), nullptr);
}

TEST_F(ContainerImplTest, ReadWriteOpenRecreatesMissingBatchDescriptors) {
  container_.reset();
  removeBatchDescriptorsFamily(dbPath());

  ContainerImpl reopened(
      dbPath(), Mode::ReadWrite, ContainerOptions{}, folly::none);
  {
    auto writer = reopened.write();
    writer.put(Family::batchDescriptors, bytes("batch"), bytes("pending"));
    writer.commit();
  }

  EXPECT_THAT(
      readFamilyValues(reopened, Family::batchDescriptors),
      ::testing::ElementsAre("pending"));
}

TEST_F(ContainerImplTest, WriteDataAfterCloseThrows) {
  container_->close();

  EXPECT_THROW(writeMeta(*container_, "k", "v"), std::runtime_error);
}

TEST_F(ContainerImplTest, FlushAfterCloseThrows) {
  container_->close();

  EXPECT_THROW(container_->flush(), std::runtime_error);
}

} // namespace facebook::glean::rocks::impl

int main(int argc, char** argv) {
  ::testing::InitGoogleTest(&argc, argv);
  const folly::Init init(&argc, &argv, folly::InitOptions().useGFlags(false));
  const int result = RUN_ALL_TESTS();
  std::fflush(stdout);
  std::fflush(stderr);
  std::_Exit(result);
}
