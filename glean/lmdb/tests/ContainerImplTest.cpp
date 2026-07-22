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
#include <vector>

#include <folly/Range.h>
#include <folly/testing/TestUtil.h>

#include "glean/lmdb/container-impl.h"

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
