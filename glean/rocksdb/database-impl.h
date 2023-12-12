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

#include "glean/rocksdb/rocksdb.h"
#include "glean/rocksdb/stats.h"
#include "glean/rocksdb/util.h"
#include "glean/rocksdb/container-impl.h"

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

struct DatabaseImpl final : Database {
  int64_t db_version;
  ContainerImpl container_;
  Id starting_id;
  Id next_id;
  rts::UnitId first_unit_id;
  rts::UsetId next_uset_id; // also next UnitId, since they share a namespace
  AtomicPredicateStats stats_;
  std::vector<size_t> ownership_unit_counters;
  folly::F14FastMap<uint64_t, size_t> ownership_derived_counters;

  // Cached ownership sets, only used when writing.
  // Note: must only be accessed under the write lock
  std::unique_ptr<rts::Usets> usets_;

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
  std::unique_ptr<rts::FactIterator>
  seek(Pid type, folly::ByteRange start, size_t prefix_size) override;
  std::unique_ptr<rts::FactIterator> seekWithinSection(
      Pid type,
      folly::ByteRange start,
      size_t prefix_size,
      Id from,
      Id upto) override;

  rts::UsetId getOwner(Id id) override;

  /// stats

  rts::PredicateStats loadStats();

  rts::PredicateStats predicateStats() const override {
    return stats_.get();
  }

  bool lookupById(Id id, rocksdb::PinnableSlice& val) const;

  std::vector<size_t> loadOwnershipUnitCounters();
  folly::F14FastMap<uint64_t, size_t> loadOwnershipDerivedCounters();
  std::unique_ptr<rts::Usets> loadOwnershipSets();

  /// Ownership

  folly::Optional<uint32_t> getUnitId(folly::ByteRange unit) override;
  folly::Optional<std::string> getUnit(uint32_t unit_id) override;

  void addOwnership(const std::vector<OwnershipSet>& ownership) override;

  std::unique_ptr<rts::DerivedFactOwnershipIterator>
  getDerivedFactOwnershipIterator(Pid pid) override;

  std::unique_ptr<rts::OwnershipUnitIterator> getOwnershipUnitIterator()
      override;

  void storeOwnership(rts::ComputedOwnership& ownership) override;

  std::unique_ptr<rts::Ownership> getOwnership() override;

  void addDefineOwnership(rts::DefineOwnership& define) override;

  // Cache for getOwner(Id)
  //
  // We start with an interval map stored in the factOwners column family. This
  // is translated into a more efficient representation in factOwnerPages when
  // the DB is finalized,
  //
  // The cache is split into pages each covering 2^PAGE_BITS Ids.
  // The prefix of a page is Id >> PAGE_BITS.
  //
  // We're trying to trade-off:
  //    - space overhead in the DB: group data into pages, and don't create
  //      DB entries for empty pages.
  //    - time to open a DB: the cache is populated lazily from the DB
  //    - latency when the cache is cold: just one Get() to fetch a page
  //    - as few DB lookups as possible: each lookup populates a whole page
  //
  // PageIndex
  //    - stored as one blob in the DB, factOwnerPages["INDEX"]
  //    - maps prefix -> maybe UsetId
  //      - UsetId => all Ids in this page map to the same UsetId, which might
  //      be INVALID_USET
  //      - nothing => fetch the page
  //    - the purpose of the index is to support the sparse interval maps we
  //    will have in
  //      stacked DBs. Otherwise we would need 250k entries in the PageStore for
  //      a stacked DB where the base has 1B facts. With the index we just need
  //      a 1MB index blob where a few of the pages will be populated.
  //
  // PageStore
  //    - stored as prefix -> Page in the DB
  //    - Each page is a table of intervals [(id1,set1), (id2,set2), ...],
  //      split into two arrays because the IDs are 16 bits and we want that
  //      array to be as compact as possible. See "struct Page" below.
  //
  // To find the UsetId for a given Id:
  //    - look up the prefix in the index
  //    - if the index contains a UsetId, that's the result
  //    - otherwise fetch the Page for this prefix
  //    - binary-search in the Page to find the correct interval

  /// Enable the fact owner cache. This should only be called when the
  // DB is read-only, and only after prepareFactOwnerCache().
  void cacheOwnership() override;

  /// Translate the data in factOwners into factOwnerPages. Do not call
  // prepareFactOwnerCache() until the DB is complete.
  void prepareFactOwnerCache() override;

  struct FactOwnerCache {
    static void prepare(ContainerImpl& container);
    void enable(ContainerImpl& container);

    // Lookup in the cache. Returns none if the cache is not enabled
    std::optional<rts::UsetId> getOwner(ContainerImpl& container, Id id);

  private:
    struct Page {
      std::vector<uint16_t> factIds;
      std::vector<rts::UsetId> setIds;
    };
    static rts::UsetId lookup(const Page& page, Id id);
    static std::unique_ptr<Page> readPage(ContainerImpl& container, uint64_t prefix);

    struct Cache {
      std::vector<rts::UsetId> index;
      std::vector<std::unique_ptr<Page>> pages;

      // tracks the memory usage of the cache
      size_t size_;
    };

    // nullptr means the cache is disabled (while the DB is writable)
    folly::Synchronized<std::unique_ptr<Cache>> cache_;
  };

  FactOwnerCache factOwnerCache_;
};

extern const char *admin_names[];

template <typename T>
folly::Optional<T> readAdminValue(
    ContainerImpl& container_,
    AdminId id) {
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

}
}
}
} // namespace facebook
