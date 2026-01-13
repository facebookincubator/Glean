/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include <folly/container/F14Map.h>

#include "glean/rts/ownership/uset.h"

#include "glean/storage/common.h"
#include "glean/storage/stats.h"

#include "glean/rocksdb/container-impl.h"
#include "glean/rocksdb/rocksdb.h"
#include "glean/rocksdb/util.h"

namespace facebook {
namespace glean {
namespace rocks {
namespace impl {

enum class AdminId : uint32_t {
  NEXT_ID,
  VERSION,
  STARTING_ID,
  FIRST_UNIT_ID,
  NEXT_UNIT_ID,
  ORPHAN_FACTS,
};

struct DatabaseImpl final : DatabaseCommon<ContainerImpl> {
  int64_t db_version;
  Id starting_id;
  Id next_id;
  AtomicPredicateStats stats_;

  explicit DatabaseImpl(
      ContainerImpl c,
      Id start,
      rts::UsetId first_unit_id,
      int64_t version);

  DatabaseImpl(const DatabaseImpl&) = delete;
  DatabaseImpl& operator=(const DatabaseImpl&) = delete;
  DatabaseImpl(DatabaseImpl&&) = delete;
  DatabaseImpl& operator=(DatabaseImpl&&) = delete;

  Container& container() noexcept override {
    return container_;
  }

  void commit(rts::FactSet& facts) override;

  /// Lookup implementation

  Id idByKey(Pid type, folly::ByteRange key) override;
  Pid typeById(Id id) override;
  bool factById(Id id, std::function<void(Pid, rts::Fact::Clause)> f) override;

  rts::Id startingId() const override {
    return starting_id;
  }

  rts::Id firstFreeId() const override {
    return next_id;
  }

  rts::Interval count(Pid pid) const override {
    return stats_.count(pid);
  }

  std::unique_ptr<rts::FactIterator> enumerate(Id from, Id upto) override;
  std::unique_ptr<rts::FactIterator> enumerateBack(Id from, Id downto) override;
  std::unique_ptr<rts::FactIterator> seek(
      Pid type,
      folly::ByteRange prefix,
      std::optional<rts::Fact::Ref> restart) override;
  std::unique_ptr<rts::FactIterator> seekWithinSection(
      Pid type,
      folly::ByteRange prefix,
      Id from,
      Id upto,
      std::optional<rts::Fact::Ref> restart) override;

  /// stats

  rts::PredicateStats loadStats();

  rts::PredicateStats predicateStats() const override {
    return stats_.get();
  }

  bool lookupById(Id id, rocksdb::PinnableSlice& val) const;

  /// Ownership (most of this is in common.h)

  OwnershipStats getOwnershipStats() override;
};

extern const char* admin_names[];

template <typename T>
folly::Optional<T> readAdminValue(ContainerImpl& container_, AdminId id) {
  container_.requireOpen();
  rocksdb::PinnableSlice val;
  binary::Output key;
  key.fixed(id);
  auto s = container_.db->Get(
      rocksdb::ReadOptions(),
      container_.family(Family::admin),
      slice(key),
      &val);
  if (!s.IsNotFound()) {
    check(s);
    binary::Input value = input(val);
    auto result = value.fixed<T>();
    if (!value.empty()) {
      rts::error(
          "corrupt database - invalid {}",
          admin_names[static_cast<uint32_t>(id)]);
    }
    return result;
  } else {
    return {};
  };
}

} // namespace impl
} // namespace rocks
} // namespace glean
} // namespace facebook
