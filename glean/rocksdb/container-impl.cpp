/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "glean/rocksdb/container-impl.h"
#include "glean/rocksdb/database-impl.h"
#include "glean/rocksdb/util.h"

#include <rocksdb/filter_policy.h>
#include <rocksdb/slice_transform.h>
#include <rocksdb/table.h>
#include <rocksdb/statistics.h>

#ifdef GLEAN_FACEBOOK
#include "glean/facebook/rocksdb/rocksdb.h"
#endif

namespace facebook {
namespace glean {
namespace rocks {
namespace impl {

std::vector<const Family *> Family::families;

const Family Family::admin("admin", [](auto& opts){
  opts.OptimizeForPointLookup(10); });
const Family Family::entities("entities", [](auto& opts){
  // NOTE: Setting inplace_update_support=true leads to rocksdb assertion
  // failures when iteration backwards.
  opts.inplace_update_support = false; });
const Family Family::keys("keys", [](auto& opts) {
  opts.prefix_extractor.reset(
    rocksdb::NewFixedPrefixTransform(sizeof(Id::word_type))); });
const Family Family::stats("stats", [](auto& opts) {
  opts.OptimizeForPointLookup(10); });
const Family Family::meta("meta", [](auto&) {});

// Maps a unit's name to its UnitId
// ownershipUnits: String -> UnitId
const Family Family::ownershipUnits("ownershipUnits", [](auto& opts) {
  opts.OptimizeForPointLookup(10); });

// Translates UnitId to its name. Dual of ownershipUnits.
// ownershipUnitIds: UnitId -> String
const Family Family::ownershipUnitIds("ownershipUnitIds", [](auto& opts) {
  opts.OptimizeForPointLookup(10); });

// Append-only log of non-derived facts' ownership information.
// Describes a map from UnitId to list of facts.
// The key contains a tuple of UnitId and a monotonically incrementing counter.
// The value is an interval map of fact ids.
// ownershipRaw: (UnitId, nat) -> [Fid]
const Family Family::ownershipRaw("ownershipRaw", [](auto&) {}, false);

// Append-only log of derived facts' ownership information.
// Describes a (Map Pid (Map Fid UsetId))
// The key contains a tuple of Pid and a monotonically incrementing counter.
// ownershipDerivedRaw: (Pid, nat) -> ( [Fid], [UsetId] )
const Family Family::ownershipDerivedRaw("ownershipDerivedRaw", [](auto& opts) {
  opts.inplace_update_support = false; }, false);

// ownershipSets: UsetId -> (Operation, [UsetId])
const Family Family::ownershipSets("ownershipSets", [](auto& opts){
  opts.inplace_update_support = false; });

// An interval map, mapping fact ids to UsetId.
// factOwners: Fid -> UsetId
const Family Family::factOwners("factOwners", [](auto& opts){
  opts.inplace_update_support = false; }, false);

// Used to efficiently map fact ids to UsetIds
// Contains:
// - A page index at key INDEX_KEY with type [UsetId]
// - A map from page prefix to a map from page suffix to UsetId
//    prefix -> ([suffix], [UsetId])
const Family Family::factOwnerPages("factOwnerPages", [](auto& opts) {
  opts.OptimizeForPointLookup(10); });

#ifndef GLEAN_FACEBOOK
namespace {
rocksdb::Status openRocksDB(
    const rocksdb::Options& options,
    const std::string& name,
    rocksdb::DB** dbptr,
    const std::vector<rocksdb::ColumnFamilyDescriptor>& column_families,
    std::vector<rocksdb::ColumnFamilyHandle*>* handles,
    bool read_only) {
  if (read_only) {
    return rocksdb::DB::OpenForReadOnly(
        options, name, column_families, handles, dbptr);
  } else {
    return rocksdb::DB::Open(options, name, column_families, handles, dbptr);
  }
}

void exportStatistics(std::shared_ptr<rocksdb::Statistics> stats) {}
} // namespace
#endif

ContainerImpl::ContainerImpl(
    const std::string& path,
    Mode m,
    bool cache_index_and_filter_blocks,
    folly::Optional<std::shared_ptr<Cache>> cache) {
  mode = m;

  if (mode == Mode::Create) {
    options.error_if_exists = true;
    options.create_if_missing = true;
  } else {
    options.error_if_exists = false;
    options.create_if_missing = false;
  }

  options.inplace_update_support = true;
  options.allow_concurrent_memtable_write = false;

  {
    rocksdb::BlockBasedTableOptions table_options;
    if (cache) {
      table_options.block_cache = std::move(cache.value());
    }
    table_options.filter_policy.reset(rocksdb::NewBloomFilterPolicy(10, false));
    table_options.whole_key_filtering = true;

    if (cache_index_and_filter_blocks) {
      table_options.cache_index_and_filter_blocks = true;
      table_options.cache_index_and_filter_blocks_with_high_priority = true;
      table_options.pin_l0_filter_and_index_blocks_in_cache = true;
    }

    // The default setting of block_size_deviation = 10 means that
    // RocksDB will always add another KV to the current block if it
    // is <90% full, even if the key is huge. We had an issue where
    // there was a large entry in the entities column family adjacent
    // to smaller entries in the same block, and because the block was
    // massive it didn't get cached, so we had very poor performance
    // for fetching all keys in that block.
    //
    // This setting means that RocksDB will never add a large K/V pair
    // to a block if it would make the block larger than the max block
    // size.
    table_options.block_size_deviation = 100;

    options.table_factory.reset(
        rocksdb::NewBlockBasedTableFactory(table_options));
  }

  // options.IncreaseParallelism();
  // options.compression = rocksdb::CompressionType::kNoCompression;
  // writeOptions.sync = false;
  // writeOptions.disableWAL = true;

  options.compression = rocksdb::CompressionType::kLZ4Compression;

  statistics = rocksdb::CreateDBStatistics();
  options.statistics = statistics;

  exportStatistics(statistics);

  families.resize(Family::count(), nullptr);
  std::vector<std::string> names;
  if (mode != Mode::Create) {
    check(rocksdb::DB::ListColumnFamilies(options, path, &names));
  }

  std::vector<rocksdb::ColumnFamilyDescriptor> existing;
  std::vector<rocksdb::ColumnFamilyHandle**> ptrs;
  for (const auto& name : names) {
    if (name != rocksdb::kDefaultColumnFamilyName) {
      if (auto family = Family::family(name)) {
        rocksdb::ColumnFamilyOptions opts(options);
        family->options(opts);
        existing.push_back(rocksdb::ColumnFamilyDescriptor(name, opts));
        ptrs.push_back(&families[family->index]);
      } else {
        rts::error("Unknown column family '{}'", name);
      }
    }
  }
  existing.push_back(rocksdb::ColumnFamilyDescriptor(
      rocksdb::kDefaultColumnFamilyName, options));
  ptrs.push_back(nullptr);

  std::vector<rocksdb::ColumnFamilyHandle*> hs;
  rocksdb::DB* db_ptr;
  check(openRocksDB(
      options, path, &db_ptr, existing, &hs, mode == Mode::ReadOnly));
  if (!db_ptr) {
    rts::error("got nullptr from rocksdb");
  } else {
    db.reset(db_ptr);
  }

  assert(hs.size() == ptrs.size());
  for (size_t i = 0; i < ptrs.size(); ++i) {
    if (ptrs[i] != nullptr) {
      *ptrs[i] = hs[i];
    } else {
      db->DestroyColumnFamilyHandle(hs[i]);
    }
  }

  for (size_t i = 0; i < families.size(); ++i) {
    if (families[i] == nullptr) {
      auto family = Family::family(i);
      assert(family != nullptr);

      rocksdb::ColumnFamilyOptions opts(options);
      family->options(opts);
      check(db->CreateColumnFamily(opts, family->name, &families[i]));
    }
  }
}

void ContainerImpl::close() noexcept {
  if (db) {
    for (auto handle : families) {
      if (handle) {
        try {
          db->DestroyColumnFamilyHandle(handle);
        } catch (const std::exception& e) {
          LOG(ERROR) << e.what();
        } catch (...) {
          LOG(ERROR) << "unknown error while closing column family";
        }
      }
    }
    families.resize(0);
    // ensure the WAL is fully sync'd, otherwise re-opening the DB will take a
    // long time due to WAL recovery.
    db->FlushWAL(true);
    db.reset();
  }
}

void ContainerImpl::requireOpen() const {
  if (!db) {
    rts::error("rocksdb: database is closed");
  }
}

rocksdb::ColumnFamilyHandle* ContainerImpl::family(const Family& family) const {
  assert(family.index < families.size());
  return families[family.index];
}

void ContainerImpl::writeData(folly::ByteRange key, folly::ByteRange value) {
  requireOpen();
  check(db->Put(writeOptions, family(Family::meta), slice(key), slice(value)));
}

bool ContainerImpl::readData(
    folly::ByteRange key,
    std::function<void(folly::ByteRange)> f) {
  requireOpen();
  rocksdb::PinnableSlice val;
  auto s =
      db->Get(rocksdb::ReadOptions(), family(Family::meta), slice(key), &val);
  if (s.IsNotFound()) {
    return false;
  } else {
    check(s);
    f(byteRange(val));
    return true;
  }
}

void ContainerImpl::optimize(bool compact) {
  for (uint32_t i = 0; i < families.size(); i++) {
    auto family = Family::family(i);
    auto handle = families[i];
    if (handle && family) {
      // For some reason backup doesn't always flush all the WAL logs, but
      // flushing here seems to fix it. If we backup the DB with WAL logs (which
      // may be many GB in size), every time the DB is opened the logs have to
      // be replayed, which takes a long time and consumes memory.
      rocksdb::FlushOptions flush_options;
      db->Flush(flush_options, handle);
      if (!family->keep) {
        // delete the contents of this column family
        check(db->DropColumnFamily(handle));
        db->DestroyColumnFamilyHandle(handle);
        rocksdb::ColumnFamilyOptions opts(options);
        family->options(opts);
        check(db->CreateColumnFamily(opts, family->name, &handle));
        families[i] = handle;
      }
      if (compact) {
        const auto nlevels = db->NumberLevels(handle);
        if (nlevels != 2) {
          rocksdb::CompactRangeOptions copts;
          check(db->CompactRange(copts, handle, nullptr, nullptr));
        }
      }
    }
  }
}

std::unique_ptr<Database> ContainerImpl::openDatabase(
    Id start,
    rts::UsetId first_unit_id,
    int32_t version) && {
  return std::make_unique<DatabaseImpl>(
      std::move(*this), start, first_unit_id, version);
}

namespace {

std::unique_ptr<rocksdb::BackupEngine> backupEngine(const std::string &path,
                                                    bool sync = true) {
  rocksdb::BackupEngine *p;
  rocksdb::BackupEngineOptions opts{path};
  opts.sync = sync;
  opts.max_background_operations = 16;
  check(rocksdb::BackupEngine::Open(rocksdb::Env::Default(), opts, &p));
  return std::unique_ptr<rocksdb::BackupEngine>(p);
}
}

void ContainerImpl::backup(const std::string& path) {
  requireOpen();
  bool flush{mode != Mode::ReadOnly};
  // no need to sync on backup: we're going to upload and delete it immediately
  check(backupEngine(path, false)->CreateNewBackup(db.get(), flush));
}

} // namespace impl

void restore(const std::string& target, const std::string& source) {
  impl::check(impl::backupEngine(source)->RestoreDBFromLatestBackup(target, target));
}

}
}
}
