/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include "glean/rocksdb/rocksdb.h"
#include "glean/rocksdb/util.h"

#include <rocksdb/db.h>
#include <rocksdb/utilities/backup_engine.h>

namespace facebook {
namespace glean {
namespace rocks {
namespace impl {

struct Family {
 private:
  template <typename F>
  Family(const char* n, F&& o, bool keep_ = true)
      : index(families.size()),
        name(n),
        options(std::forward<F>(o)),
        keep(keep_) {
    families.push_back(this);
  }

  Family(const Family&) = delete;
  Family& operator=(const Family&) = delete;
  ~Family() = default;
  Family(Family&&) = delete;
  Family& operator=(Family&&) = delete;

  static std::vector<const Family*> families;

 public:
  size_t index;
  const char* name;
  std::function<void(rocksdb::ColumnFamilyOptions&)> options;

  // Whether to keep this column family after the DB is complete. If
  // keep = false, then the contents of the column family will be
  // deleted before compaction.
  bool keep = true;

  static const Family admin;
  static const Family entities;
  static const Family keys;
  static const Family stats;
  static const Family meta;
  static const Family ownershipUnits;
  static const Family ownershipUnitIds;
  static const Family ownershipRaw;
  static const Family ownershipDerivedRaw;
  static const Family ownershipSets;
  static const Family factOwners;
  static const Family factOwnerPages;
  static const Family batchDescriptors;

  static size_t count() {
    return families.size();
  }

  static const Family* FOLLY_NULLABLE family(const std::string& name) {
    for (auto p : families) {
      if (name == p->name) {
        return p;
      }
    }
    return nullptr;
  }

  static const Family* FOLLY_NULLABLE family(size_t i) {
    return i < families.size() ? families[i] : nullptr;
  }
};

struct Iterator {
  Iterator() {}

  Iterator(rocksdb::DB* db, rocksdb::ColumnFamilyHandle* f)
      : iter(db->NewIterator(rocksdb::ReadOptions(), f)) {
    if (!iter) {
      rts::error("rocksdb: couldn't allocate iterator");
    }
  }

  void seek_first() {
    iter->SeekToFirst();
  }
  void seek_last() {
    iter->SeekToLast();
  }
  void seek_key(folly::ByteRange key) {
    iter->Seek(slice(key));
  }
  void seek_key_lower(folly::ByteRange key) {
    iter->SeekForPrev(slice(key));
  };

  void next() {
    iter->Next();
  }
  void prev() {
    iter->Prev();
  }

  bool valid() {
    return iter->Valid();
  }

  folly::ByteRange key() {
    return byteRange(iter->key());
  }
  folly::ByteRange value() {
    return byteRange(iter->value());
  }

 private:
  std::unique_ptr<rocksdb::Iterator> iter;
};

struct ContainerImpl final : Container {
  Mode mode;
  rocksdb::Options options;
  rocksdb::WriteOptions writeOptions;
  std::unique_ptr<rocksdb::DB> db;
  std::vector<rocksdb::ColumnFamilyHandle*> families;
  std::shared_ptr<rocksdb::Statistics> statistics;

  using Family = impl::Family;
  using Iterator = impl::Iterator;

  ContainerImpl(
      const std::string& path,
      Mode m,
      bool cache_index_and_filter_blocks,
      folly::Optional<std::shared_ptr<Cache>> cache);

  ContainerImpl(const ContainerImpl&) = delete;
  ContainerImpl(ContainerImpl&& other) = default;
  ContainerImpl& operator=(const ContainerImpl&) = delete;
  ContainerImpl& operator=(ContainerImpl&&) = delete;

  ~ContainerImpl() override {
    close();
  }

  void close() noexcept override;

  void requireOpen() const;

  void backup(const std::string& path) override;
  std::unique_ptr<Database>
      openDatabase(Id start, rts::UsetId first_unit_id, int32_t version) &&
      override;

  void writeData(folly::ByteRange key, folly::ByteRange value) override;

  bool readData(folly::ByteRange key, std::function<void(folly::ByteRange)> f)
      override {
    return get(Family::meta, key, f);
  }

  void optimize(bool compact) override;

  rocksdb::ColumnFamilyHandle* family(const Family& family) const;

  Iterator read(const Family& f) {
    return Iterator(db.get(), family(f));
  }

  template <typename F>
  bool get(const Family& fam, folly::ByteRange key, F&& f) {
    rocksdb::PinnableSlice val;
    auto s = db->Get(rocksdb::ReadOptions(), family(fam), slice(key), &val);
    if (s.IsNotFound()) {
      return false;
    } else {
      check(s);
      std::forward<F>(f)(byteRange(val));
      return true;
    }
  }

  struct Writer {
    Writer(ContainerImpl& c, unsigned int flags = 0) : container_(c) {}

    void put(const Family& f, folly::ByteRange k, folly::ByteRange v) {
      check(batch.Put(container_.family(f), slice(k), slice(v)));
    }

    void commit() {
      check(container_.db->Write(container_.writeOptions, &batch));
    }

   private:
    ContainerImpl& container_;
    rocksdb::WriteBatch batch;
  };

  Writer write() {
    return Writer(*this);
  }
};

} // namespace impl
} // namespace rocks
} // namespace glean
} // namespace facebook
