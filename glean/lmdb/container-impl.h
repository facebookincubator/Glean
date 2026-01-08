/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include "glean/storage/db.h"
#include "glean/lmdb/lmdb.h"
#include "glean/lmdb/util.h"

namespace facebook {
namespace glean {
namespace lmdb {
namespace impl {

using namespace facebook::glean::db;

struct Family {
 private:
  Family(const char* n, unsigned int flags_, bool keep_ = true)
      : index(families.size()),
        name(n),
        flags(flags_),
        keep(keep_) {
    families.push_back(this);
  }

  Family(const Family&) = delete;
  Family& operator=(const Family&) = delete;

  static std::vector<const Family*> families;

 public:
  size_t index;
  const char* name;
  unsigned int flags;

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

/// A transaction-cursor pair
struct Iterator : Cursor {
    Iterator(Txn txn, MDB_dbi dbi) :
        Cursor(txn.cursor(dbi)), txn_(std::move(txn)) {}

  private:
    Txn txn_;
};

struct ContainerImpl final : Container {
  Mode mode;
  std::unique_ptr<MDB_env, decltype(&mdb_env_close)> db;
  std::vector<MDB_dbi> families;
  size_t key_size;

  using Family = impl::Family;
  using Iterator = impl::Iterator;

  ContainerImpl(const std::string &path, Mode m);

  ContainerImpl(const ContainerImpl &) = delete;
  ContainerImpl(ContainerImpl &&other) = default;
  ContainerImpl &operator=(const ContainerImpl &) = delete;
  ContainerImpl &operator=(ContainerImpl &&) = delete;

  ~ContainerImpl() override { close(); }

  void close() noexcept override;

  void requireOpen() const;

  void backup(const std::string &path) override;
  static void restore(const std::string& target, const std::string& source);

  std::unique_ptr<Database> openDatabase(Id start, rts::UsetId first_unit_id,
                                         int32_t version) &&
      override;

  void writeData(folly::ByteRange key, folly::ByteRange value) override;

  bool readData(folly::ByteRange key,
                std::function<void(folly::ByteRange)> f) override {
    return get(Family::meta, key, f);
  }

  void optimize(bool compact) override;

  MDB_dbi family(const Family &family) const;

  Txn txn_write() { return Txn(db.get()); };

  Txn txn_read() { return Txn(db.get(), MDB_RDONLY); };

  Iterator read(const Family &f) { return Iterator(txn_read(), family(f)); }

  template <typename F>
  bool get(const Family &fam, folly::ByteRange key, F&& f) {
    Txn txn(db.get(), MDB_RDONLY);
    MDB_val val;
    MDB_val k = mdbVal(key);
    int s = mdb_get(txn.ptr(), family(fam), &k, &val);
    if (s == MDB_SUCCESS) {
      std::forward<F>(f)(byteRange(val));
      return true;
    } else if (s == MDB_NOTFOUND) {
      return false;
    } else {
      check(s);
      return false;
    }
  }

  struct Writer {
    Writer(ContainerImpl &c, unsigned int flags = 0)
        : container_(c), txn(nullptr, &mdb_txn_abort) {
      MDB_txn *t;
      check(mdb_txn_begin(c.db.get(), NULL, flags, &t));
      txn.reset(t);
    }

    void put(const Family &f, folly::ByteRange k, folly::ByteRange v) {
      MDB_val key = mdbVal(k);
      MDB_val val = mdbVal(v);
      check(mdb_put(txn.get(), container_.family(f), &key, &val, 0));
    }

    void commit() {
      if (txn) {
        check(mdb_txn_commit(txn.release()));
      }
    }

  private:
    ContainerImpl &container_;
    std::unique_ptr<MDB_txn, decltype(&mdb_txn_abort)> txn;
  };

  Writer write() { return Writer(*this); }
};

} // namespace impl
} // namespace lmdb
} // namespace glean
} // namespace facebook
