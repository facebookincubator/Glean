/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

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
  STARTING_ID
};

struct DatabaseImpl final : Database {
  int64_t db_version;
  ContainerImpl container_;
  Id starting_id;
  Id next_id;
  AtomicPredicateStats stats_;
  std::vector<size_t> ownership_unit_counters;
  folly::F14FastMap<uint64_t, size_t> ownership_derived_counters;

  // Cached ownership sets, only used when writing.
  // TODO: initialize this lazily
  std::unique_ptr<rts::Usets> usets_;

  explicit DatabaseImpl(ContainerImpl c, Id start, int64_t version);

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

  /// Enable the fact owner cache. This should only be called when the
  // DB is read-only.
  void cacheOwnership() override;

  // Cache for getOwner(Id). This is represented as a vector of pages
  // indexed by the upper 48 bits of the fact ID. Each page is a table
  // of intervals [(id1,set1), (id2,set2), ...], split into two arrays
  // because the IDs are 16 bits and we want that array to be as
  // compact as possible. We find the correct interval in getOwner()
  // by binary search in the page on the low 16 bits of the fact ID.
  // TODO: maybe it's worth optimizing very dense pages to be just a
  // lookup, and conversely very sparse pages to a linear search?

  struct FactOwnerCache {
    rts::UsetId getOwner(ContainerImpl& container, Id id);
    void enable();

   private:
    struct Page {
      std::vector<uint16_t> factIds;
      std::vector<rts::UsetId> setIds;
    };
    using Cache = std::vector<std::unique_ptr<Page>>;

    const Page* getPage(ContainerImpl& container, uint64_t prefix);

    // nullptr means the cache is disabled (while the DB is writable)
    folly::Synchronized<std::unique_ptr<Cache>> cache_;
    size_t size_;
  };

  FactOwnerCache factOwnerCache_;
};

}
}
}
} // namespace facebook
