/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include "glean/rts/binary.h"
#include "glean/rts/error.h"
#include "glean/lmdb-clib/lmdb.h"

namespace facebook {
namespace glean {
namespace lmdb {
namespace impl {

[[noreturn]] inline void error(int s) {
  rts::error("lmdb: {}", mdb_strerror(s));
}

inline void check(int status) {
  if (status != MDB_SUCCESS) {
    error(status);
  }
}

inline MDB_val mdbVal(folly::ByteRange r) {
  return {r.size(), const_cast<unsigned char *>(r.data())};
}

inline folly::ByteRange byteRange(MDB_val v) {
  return folly::ByteRange(const_cast<const unsigned char *>(
                              reinterpret_cast<unsigned char *>(v.mv_data)),
                          v.mv_size);
}

struct Cursor;

// A transaction, normally created by container.txn_read() or
// container.txn_write().  For a read transaction you can just let it
// go out of scope and mdb_txn_abort() will be called for you; for a
// write transaction you should call txn.commit() at the end of
// writing.

struct Txn {
  Txn(MDB_env *env, unsigned int flags = 0) : txn(nullptr, &mdb_txn_abort) {
    MDB_txn *t;
    check(mdb_txn_begin(env, NULL, flags, &t));
    txn.reset(t);
  }

  bool get(MDB_dbi db, folly::ByteRange k, folly::ByteRange &v) {
    MDB_val key = mdbVal(k);
    MDB_val val;
    int s = mdb_get(txn.get(), db, &key, &val);
    if (s == MDB_SUCCESS) {
      v = byteRange(val);
      return true;
    } else if (s == MDB_NOTFOUND) {
      return false;
    } else {
      check(s);
      return false;
    }
  }

  void put(MDB_dbi db, folly::ByteRange k, folly::ByteRange v) {
    MDB_val key = mdbVal(k), val = mdbVal(v);
    check(mdb_put(txn.get(), db, &key, &val, 0));
  }

  Cursor cursor(MDB_dbi dbi);

  MDB_txn *ptr() { return txn.get(); }

  void commit() {
    if (txn) {
      check(mdb_txn_commit(txn.release()));
    }
  }

private:
  std::unique_ptr<MDB_txn, decltype(&mdb_txn_abort)> txn;
};

// A cursor for scanning DB entries, created by txn.cursor().

struct Cursor {
  Cursor(MDB_txn *txn, MDB_dbi dbi) : cursor(nullptr, &mdb_cursor_close) {
    MDB_cursor *c;
    check(mdb_cursor_open(txn, dbi, &c));
    cursor.reset(c);
  }

  bool seek_first() { return seek_op(MDB_FIRST); }

  bool seek_last() { return seek_op(MDB_LAST); }

  bool seek_op(MDB_cursor_op op) {
    int s = mdb_cursor_get(cursor.get(), &key_, &value_, op);
    if (s == MDB_SUCCESS) {
      valid_ = true;
    } else if (s == MDB_NOTFOUND) {
      valid_ = false;
    } else {
      check(s);
    }
    return valid_;
  }

  bool seek_key(folly::ByteRange key) {
    key_ = mdbVal(key);
    return seek_op(MDB_SET_RANGE);
  }

  // Seek to the greatest item less than or equal to the key
  bool seek_key_lower(folly::ByteRange k) {
    if (!seek_key(k)) {
      return seek_last();
    } else if (key() != k) {
      return prev();
    }
    return true;
  }

  // only for MDB_DUPSORT
  bool seek_both(folly::ByteRange key, folly::ByteRange value) {
    key_ = mdbVal(key);
    value_ = mdbVal(value);
    if (seek_op(MDB_GET_BOTH)) {
        // apparently doesn't set key/value, therefore we need:
        return seek_op(MDB_GET_CURRENT);
    } else {
        return false;
    }
  }

  bool next() { return seek_op(MDB_NEXT); }
  bool next_dup() { return seek_op(MDB_NEXT_DUP); }

  bool prev() { return seek_op(MDB_PREV); }
  bool prev_dup() { return seek_op(MDB_PREV_DUP); }

  bool valid() { return valid_; }

  folly::ByteRange key() { return byteRange(key_); }
  folly::ByteRange value() { return byteRange(value_); }

  MDB_cursor *ptr() { return cursor.get(); }

private:
  std::unique_ptr<MDB_cursor, decltype(&mdb_cursor_close)> cursor;
  MDB_val key_, value_;
  bool valid_;
};

inline Cursor Txn::cursor(MDB_dbi dbi) {
  return Cursor(txn.get(), dbi);
}

} // namespace impl
} // namespace lmdb
} // namespace glean
} // namespace facebook
