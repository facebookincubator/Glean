/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include "glean/rts/densemap.h"
#include "glean/rts/factset.h"
#include "glean/rts/lookup.h"
#include "glean/rts/ownership.h"
#include "glean/rts/ownership/derived.h"
#include "glean/rts/stats.h"
#include "glean/rts/store.h"

namespace rocksdb {
struct Cache;
struct Iterator;
}

namespace facebook{
namespace glean {
namespace rocks {

using rts::Id;
using rts::Pid;

using Cache = ::rocksdb::Cache;

std::shared_ptr<Cache> newCache(
  size_t capacity
);

struct Database;

/// A rocksdb container for storing facts
struct Container {
  virtual ~Container() {}

  /// Close the 'Container' - accessing it afterwards isn't allowed.
  virtual void close() noexcept = 0;

  /// Write a key/value pair to the Container metadata.
  virtual void writeData(folly::ByteRange key, folly::ByteRange value) = 0;

  /// Lookup a key in the Container metadata.
  virtual bool readData(
    folly::ByteRange key, std::function<void(folly::ByteRange)> f) = 0;

  /// Optimise the container for reading
  virtual void optimize(bool compact) = 0;

  /// Backup the Container to the specified directory.
  virtual void backup(const std::string& path) = 0;

  /// Convert the Container to a full fact Database with the given
  /// representation version - accessing the original Container afterwards isn't
  /// allowed. If the database is being created, start is the starting fact id.
  ///
  /// The base Ownership passed in (if any) must not be destructed before the
  /// Database.
  virtual std::unique_ptr<Database>
  openDatabase(Id start, rts::UsetId first_unit_id, int32_t version) && = 0;
};

enum class Mode {
  ReadOnly = 0,
  ReadWrite = 1,
  Create = 2
};

std::unique_ptr<Container> open(
  const std::string& path,
  Mode mode,
  bool cache_index_and_filter_blocks,
  folly::Optional<std::shared_ptr<Cache>> cache);

/// A rocksdb-based fact database
struct Database : rts::Lookup {
  virtual Container& container() noexcept = 0;

  virtual rts::PredicateStats predicateStats() const = 0;

  struct OwnershipSet {
    folly::ByteRange unit;
    folly::Range<const int64_t *> ids;
      // This is a list of intervals [x1,x2, y1,y2, ...]
      // representing the inclusive ranges x1..x2, y1..y2, ...
  };

  virtual void commit(rts::FactSet& facts) = 0;

  virtual void addOwnership(const std::vector<OwnershipSet>& ownership) = 0;
  virtual std::unique_ptr<rts::OwnershipUnitIterator>
    getOwnershipUnitIterator() = 0;

  virtual void addDefineOwnership(rts::DefineOwnership& def) = 0;
  virtual std::unique_ptr<rts::DerivedFactOwnershipIterator>
    getDerivedFactOwnershipIterator(Pid pid) = 0;

  virtual folly::Optional<uint32_t> getUnitId(folly::ByteRange) = 0;
  virtual folly::Optional<std::string> getUnit(uint32_t) = 0;

  virtual void storeOwnership(rts::ComputedOwnership &ownership) = 0;
  virtual std::unique_ptr<rts::Ownership> getOwnership() = 0;

  virtual void cacheOwnership() = 0;
};

void restore(const std::string& target, const std::string& source);

}
}
}
