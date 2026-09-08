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
#include <optional>
#include <string>
#include <vector>

#include <folly/Range.h>
#include <folly/init/Init.h>
#include <folly/testing/TestUtil.h>
#include <rocksdb/db.h>

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
// Container: open/read/write against a real on-disk RocksDB
// ============================================================

class ContainerImplTest : public ::testing::Test {
 protected:
  void SetUp() override {
    tmpDir_ = std::make_unique<folly::test::TemporaryDirectory>();
    container_ = std::make_unique<ContainerImpl>(
        dbPath(),
        Mode::Create,
        /*cache_index_and_filter_blocks=*/false,
        folly::none);
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

TEST_F(ContainerImplTest, DataPersistsAfterReopeningReadOnly) {
  writeMeta(*container_, "persisted", "value");
  container_.reset();

  ContainerImpl reopened(
      dbPath(),
      Mode::ReadOnly,
      /*cache_index_and_filter_blocks=*/false,
      folly::none);
  const auto v = readMeta(reopened, "persisted");
  ASSERT_TRUE(v.has_value());
  EXPECT_EQ(*v, "value");
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

TEST_F(ContainerImplTest, FlushThenBackupProducesReadableCopy) {
  writeMeta(*container_, "backed-up", "value");
  container_->flush();

  const std::string backupPath = tmpDir_->path().string() + "/backup";
  std::filesystem::create_directory(backupPath);
  container_->backup(backupPath);
  container_.reset();

  const std::string restoredPath = tmpDir_->path().string() + "/restored";
  restore(restoredPath, backupPath);

  ContainerImpl restored(
      restoredPath,
      Mode::ReadOnly,
      /*cache_index_and_filter_blocks=*/false,
      folly::none);
  const auto v = readMeta(restored, "backed-up");
  ASSERT_TRUE(v.has_value());
  EXPECT_EQ(*v, "value");
}

TEST_F(ContainerImplTest, ReadOnlyCheckpointProducesOpenableSnapshot) {
  writeMeta(*container_, "checkpointed", "value");
  container_->flush();
  container_.reset();

  ContainerImpl source(
      dbPath(),
      Mode::ReadOnly,
      /*cache_index_and_filter_blocks=*/false,
      folly::none);
  const std::string checkpointPath = tmpDir_->path().string() + "/checkpoint";
  source.checkpoint(checkpointPath);

  ContainerImpl checkpoint(
      checkpointPath,
      Mode::ReadOnly,
      /*cache_index_and_filter_blocks=*/false,
      folly::none);
  const auto v = readMeta(checkpoint, "checkpointed");
  ASSERT_TRUE(v.has_value());
  EXPECT_EQ(*v, "value");
}

TEST_F(ContainerImplTest, ReadOnlyOpenSucceedsWhenOldDbLacksBatchDescriptors) {
  writeMeta(*container_, "old-db", "value");
  container_.reset();
  removeBatchDescriptorsFamily(dbPath());

  ContainerImpl reopened(
      dbPath(),
      Mode::ReadOnly,
      /*cache_index_and_filter_blocks=*/false,
      folly::none);
  const auto v = readMeta(reopened, "old-db");
  ASSERT_TRUE(v.has_value());
  EXPECT_EQ(*v, "value");
  EXPECT_EQ(reopened.family(Family::batchDescriptors), nullptr);
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
