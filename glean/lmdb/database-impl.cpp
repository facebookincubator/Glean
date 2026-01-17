/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "glean/lmdb/database-impl.h"
#include "glean/lmdb/container-impl.h"

#include "glean/rts/timer.h"

namespace facebook {
namespace glean {
namespace lmdb {
namespace impl {

using namespace rts;

const char* admin_names[] = {
    "NEXT_ID",
    "VERSION",
    "STARTING_ID",
    "FIRST_UNIT_ID",
    "NEXT_UNIT_ID",
    "ORPHAN_FACTS",
};

namespace {

template <typename T, typename F>
T initAdminValue(
    ContainerImpl& container_,
    AdminId id,
    T def,
    bool write,
    F&& notFound) {
  auto current = readAdminValue<T>(container_, id);
  if (current.hasValue()) {
    return *current;
  } else {
    notFound();
    if (write) {
      binary::Output key;
      key.fixed(id);
      binary::Output value;
      value.fixed(def);
      auto writer = container_.write();
      writer.put(Family::admin, key.bytes(), value.bytes());
      writer.commit();
    }
    return def;
  }
}

} // namespace

DatabaseImpl::DatabaseImpl(
    ContainerImpl c,
    Id start,
    UsetId first_unit_id_,
    int64_t version)
    : DatabaseCommon(std::move(c)) {
  starting_id = Id::fromWord(initAdminValue(
      container_,
      AdminId::STARTING_ID,
      start.toWord(),
      container_.mode == Mode::Create,
      [] {}));

  next_id = Id::fromWord(initAdminValue(
      container_,
      AdminId::NEXT_ID,
      start.toWord(),
      container_.mode == Mode::Create,
      [mode = container_.mode] {
        if (mode != Mode::Create) {
          rts::error("corrupt database - missing NEXT_ID");
        }
      }));

  first_unit_id = initAdminValue(
      container_,
      AdminId::FIRST_UNIT_ID,
      first_unit_id_,
      container_.mode == Mode::Create,
      [] {
        // TODO: later this should be an error, for now we have to be
        // able to open old DBs.
      });
  VLOG(1) << folly::sformat("first_unit_id: {}", first_unit_id);

  next_uset_id = initAdminValue(
      container_,
      AdminId::NEXT_UNIT_ID,
      first_unit_id,
      container_.mode == Mode::Create,
      [] {
        // TODO: later this should be an error, for now we have to be
        // able to open old DBs.
      });
  VLOG(1) << folly::sformat("next_uset_id: {}", next_uset_id);

  db_version = initAdminValue(
      container_,
      AdminId::VERSION,
      version,
      container_.mode == Mode::Create,
      [] {});

  if (db_version != version) {
    rts::error("unexpected database version {}", db_version);
  }

  stats_.set(loadStats());

  if (container_.mode != Mode::ReadOnly) {
    // These are only used when writing
    ownership_unit_counters = loadOwnershipUnitCounters();
    ownership_derived_counters = loadOwnershipDerivedCounters();

    // We only need usets_ for writable DBs, and it takes time and
    // memory to load them so omit this for ReadOnly DBs.
    usets_ = loadOwnershipSets();
  }

  // Enable the fact owner cache when the DB is read-only
  if (container_.mode == Mode::ReadOnly) {
    cacheOwnership();
  }
}

rts::PredicateStats DatabaseImpl::loadStats() {
  container_.requireOpen();
  rts::PredicateStats stats;
  auto iter = container_.read(Family::stats);

  for (iter.seek_first(); iter.valid(); iter.next()) {
    binary::Input key(iter.key());
    stats[key.fixed<Pid>()] = fromBytes<MemoryStats>(iter.value());
    assert(key.empty());
  }
  return stats;
}

namespace {

rts::Fact::Ref decomposeFact(Id id, const folly::ByteRange data) {
  binary::Input inp(data);
  const auto ty = inp.packed<Pid>();
  const auto key_size = inp.packed<uint32_t>();
  return rts::Fact::Ref{id, ty, rts::Fact::Clause::from(inp.bytes(), key_size)};
}

} // namespace

Id DatabaseImpl::idByKey(Pid type, folly::ByteRange key) {
  if (count(type).high() == 0) {
    return Id::invalid();
  }

  container_.requireOpen();
  binary::Output k;
  k.fixed(type);

  Txn txn = container_.txn_read();
  folly::ByteRange out;
  size_t key_space = container_.key_size - sizeof(type);
  if (key.size() <= key_space) {
    k.put(key);
    if (!txn.get(container_.family(Family::keys), k.bytes(), out)) {
      return Id::invalid();
    } else {
      binary::Input value(out);
      auto id = value.fixed<Id>();
      assert(value.empty());
      return id;
    }
  } else {
    k.bytes(key.begin(), key_space);
    auto iter = txn.cursor(container_.family(Family::keys));
    iter.seek_key(k.bytes());
    while (iter.valid()) {
        binary::Input value(iter.value());
        auto id = value.fixed<Id>();
        assert(value.empty());
        folly::ByteRange val;
        [[maybe_unused]] auto found = lookupById(txn, id, val);
        assert(found);
        auto fact = decomposeFact(id, val);
        if (key == fact.key()) {
            return id;
        }
        iter.next_dup();
    }
    return Id::invalid();
  }
}

Pid DatabaseImpl::typeById(Id id) {
  container_.requireOpen();
  folly::ByteRange val;
  Txn txn = container_.txn_read();
  if (lookupById(txn, id, val)) {
    return binary::Input(val).packed<Pid>();
  } else {
    return Pid::invalid();
  }
}

bool DatabaseImpl::factById(Id id, std::function<void(Pid, Fact::Clause)> f) {
  container_.requireOpen();
  folly::ByteRange val;
  Txn txn = container_.txn_read();
  if (lookupById(txn, id, val)) {
    auto ref = decomposeFact(id, val);
    f(ref.type, ref.clause);
    return true;
  } else {
    return false;
  }
}

bool DatabaseImpl::lookupById(Txn &txn, Id id, folly::ByteRange& val) {
  if (id < startingId() || id >= firstFreeId()) {
    return false;
  }
  binary::Output key;
  key.nat(id.toWord());
  return txn.get(container_.family(Family::entities), key.bytes(), val);
}

namespace {

struct SeekIterator final : rts::FactIterator {
  SeekIterator(
      folly::ByteRange start,
      size_t prefix_size,
      Pid type,
      Id start_id,
      DatabaseImpl* db)
      : lower_bound_({start.data(), start.data() + prefix_size}),
        upper_bound_(
            binary::lexicographicallyNext({start.data(), prefix_size})),
        type_(type),
        txn_(db->container_.txn_read()),
        iter_(txn_.cursor(db->container_.family(Family::keys))),
        db_(db) {
    size_t key_space = db->container_.key_size;
    if (start.size() > key_space && start_id) {
        // When restarting an iterator, we can jump to the right place
        // even if the key is larger than our max key size, because we
        // know the value (the fact ID) and LMDB provides a way to jump
        // to a particular key/value pair (MDB_GET_BOTH).
        if (!iter_.seek_both({start.data(), key_space}, bytesOf(start_id))) {
            rts::error("seek restart failed");
        }
    } else {
        iter_.seek_key(start);
    }
  }

  void next() override {
    iter_.next();
  }

  Fact::Ref get(Demand demand) override {
    big_key = {};
    for (; iter_.valid(); iter_.next()) {
      if (iter_.key() >= binary::byteRange(upper_bound_)) {
          return Fact::Ref::invalid();
      }
      binary::Input key(iter_.key());
      binary::Input value(iter_.value());
      bool key_overflow = key.size() >= db_->container_.key_size;
      [[maybe_unused]] auto ty = key.fixed<Pid>();
      DCHECK_EQ(ty.toWord(), type_.toWord());
      auto id = value.fixed<Id>();
      assert(value.empty());
      if (demand == KeyOnly && !key_overflow) {
          return Fact::Ref{id, type_, Fact::Clause::fromKey(key.bytes())};
      } else {
          [[maybe_unused]] auto found = db_->lookupById(txn_, id, slice_);
          assert(found);
          auto fact = decomposeFact(id, slice_);
          if (key_overflow) {
              folly::ByteRange lower_bound_key_ =
                  binary::byteRange(lower_bound_).subpiece(sizeof(Pid));
              if (lower_bound_.size() > db_->container_.key_size &&
                  fact.key() < lower_bound_key_) {
                  continue;
              }
              folly::ByteRange upper_bound_key_ =
                  binary::byteRange(upper_bound_).subpiece(sizeof(Pid));
              if (upper_bound_.size() > db_->container_.key_size &&
                  fact.key() >= upper_bound_key_) {
                  continue;
              }
          }
          return fact;
      }
    }
    return Fact::Ref::invalid();
  }

  std::optional<Id> lower_bound() override {
    return std::nullopt;
  }
  std::optional<Id> upper_bound() override {
    return std::nullopt;
  }

  std::vector<unsigned char> lower_bound_;
  std::vector<unsigned char> upper_bound_;
  binary::Output big_key;
  const Pid type_;
  Txn txn_;
  Cursor iter_;
  DatabaseImpl* db_;
  folly::ByteRange slice_;
};

} // namespace

std::unique_ptr<rts::FactIterator>
DatabaseImpl::seek(
    Pid type,
    folly::ByteRange prefix,
    std::optional<rts::Fact::Ref> restart) {
  if (count(type).high() == 0) {
    return std::make_unique<EmptyIterator>();
  }

  container_.requireOpen();
  binary::Output out;
  out.fixed(type);
  const auto type_size = out.size();
  Id start_id = Id::invalid();

  if (restart.has_value()) {
      out.put(restart.value().clause.key());
      start_id = restart.value().id;
  } else {
      out.put(prefix);
  }
  return std::make_unique<SeekIterator>(
      out.bytes(), type_size + prefix.size(), type, start_id, this);
}

std::unique_ptr<rts::FactIterator> DatabaseImpl::seekWithinSection(
    Pid type,
    folly::ByteRange prefix,
    Id from,
    Id upto,
    std::optional<rts::Fact::Ref> restart) {
  if (upto <= startingId() || firstFreeId() <= from) {
    return std::make_unique<EmptyIterator>();
  }

  return Section(this, from, upto).seek(type, prefix, restart);
}

namespace {

template<typename Direction>
struct EnumerateIterator final : rts::FactIterator {
  explicit EnumerateIterator(Id start, Id bound, DatabaseImpl* db)
      : bound_(bound),
        txn_(db->container_.txn_read()),
        iter_(txn_.cursor(db->container_.family(Family::entities))) {
    EncodedNat st(start.toWord());
    iter_.seek_key(st.byteRange());
  }

  void next() override {
    iter_.seek_op(Direction::next);
  }

  Fact::Ref get(Demand /*unused*/) override {
    if (iter_.valid()) {
        Id id = Id::fromWord(loadTrustedNat(
                                 reinterpret_cast<const unsigned char*>(
                                   iter_.key().data()))
                               .first);
        if (Direction::inside(id, bound_)) {
            return decomposeFact(id,iter_.value());
        }
    }
    return Fact::Ref::invalid();
  }

  std::optional<Id> lower_bound() override {
    return std::nullopt;
  }
  std::optional<Id> upper_bound() override {
    return std::nullopt;
  }

  Id bound_;
  Txn txn_;
  Cursor iter_;
};

struct Forward {
  static std::pair<Id, Id>
  bounds(Id from, Id upto, Id starting_id, Id next_id) {
    if (from >= next_id || (upto && upto <= starting_id)) {
      return {Id::invalid(), Id::invalid()};
    } else {
      return {
          std::max(from, starting_id),
          upto && upto <= next_id ? upto : next_id};
    }
  }

  static bool inside(Id cur, Id bound) { return cur < bound; }
  static inline constexpr auto next = MDB_NEXT;
};

struct Backward {
  static std::pair<Id, Id>
  bounds(Id from, Id downto, Id starting_id, Id next_id) {
    if (downto >= next_id || (from && from <= starting_id)) {
      return {Id::invalid(), Id::invalid()};
    } else {
      return {
          (from && from <= next_id ? from : next_id) - 1,
          std::max(downto, starting_id)};
    }
  }

  static bool inside(Id cur, Id bound) { return cur >= bound; }
  static inline constexpr auto next = MDB_PREV;
};

} // namespace

template <typename Direction>
std::unique_ptr<rts::FactIterator>
makeEnumerateIterator(DatabaseImpl* db, Id from, Id to) {
  db->container_.requireOpen();
  const auto [start, bound] =
      Direction::bounds(from, to, db->startingId(), db->firstFreeId());
  if (!start) {
    return std::make_unique<rts::EmptyIterator>();
  } else {
    return std::make_unique<EnumerateIterator<Direction>>(start, bound, db);
  }
}

std::unique_ptr<rts::FactIterator> DatabaseImpl::enumerate(Id from, Id upto) {
  return makeEnumerateIterator<Forward>(this, from, upto);
}

std::unique_ptr<rts::FactIterator> DatabaseImpl::enumerateBack(
    Id from,
    Id downto) {
  return makeEnumerateIterator<Backward>(this, from, downto);
}

void DatabaseImpl::commit(rts::FactSet& facts) {
  container_.requireOpen();

  if (facts.empty()) {
    return;
  }

  if (facts.startingId() < next_id) {
    rts::error(
        "batch inserted out of sequence ({} < {})",
        facts.startingId(),
        next_id);
  }

  auto writer = container_.write();

  // NOTE: We do *not* support concurrent writes so we don't need to protect
  // stats_ here because nothing should be able to replace it while we're
  // running
  const auto& old_stats = stats_.unprotected();
  PredicateStats new_stats(old_stats);

  for (auto iter = facts.enumerate(); auto fact = iter->get(); iter->next()) {
    assert(fact.id >= next_id);

    uint64_t mem = 0;
    auto put = [&](const auto& family, const auto& key, const auto& value) {
      writer.put(family, key, value);
      mem += key.size();
      mem += value.size();
    };

    {
      binary::Output k;
      k.nat(fact.id.toWord());
      binary::Output v;
      v.packed(fact.type);
      v.packed(fact.clause.key_size);
      v.put({fact.clause.data, fact.clause.size()});

      put(Family::entities, k.bytes(), v.bytes());
    }

    {
      binary::Output k;
      binary::Output v;
      k.fixed(fact.type);

      auto key = fact.key();
      size_t keySpace = container_.key_size - sizeof(fact.type);
      if (key.size() > keySpace) {
          k.bytes(key.begin(), keySpace);
      } else {
          k.put(key);
      }
      v.fixed(fact.id);

      put(Family::keys, k.bytes(), v.bytes());
    }

    new_stats[fact.type] += MemoryStats::one(mem);
  }

  const auto first_free_id = facts.firstFreeId();
  writer.put(
      Family::admin,
      bytesOf(AdminId::NEXT_ID),
      bytesOf(first_free_id));

  for (const auto& x : new_stats) {
    if (x.second != old_stats.get(x.first)) {
      writer.put(
          Family::stats,
          bytesOf(x.first.toWord()),
          bytesOf(x.second));
    }
  }

  writer.commit();
  next_id = first_free_id;

  stats_.set(std::move(new_stats));
}

rts::OwnershipStats DatabaseImpl::getOwnershipStats() {
  uint64_t units_size, unit_ids_size, sets_size, owners_size, num_owners,
      owner_pages_size, num_owner_pages;
  MDB_stat stat;
  Txn txn = container_.txn_read();

  auto stat_db_size = [&](MDB_stat& stat) -> size_t {
      return (size_t)stat.ms_psize * (
          stat.ms_branch_pages + stat.ms_leaf_pages + stat.ms_overflow_pages);
  };

  mdb_stat(txn.ptr(),
           container_.family(Family::ownershipUnits),
           &stat);
  units_size = stat_db_size(stat);

  mdb_stat(txn.ptr(),
           container_.family(Family::ownershipUnitIds),
           &stat);
  unit_ids_size = stat_db_size(stat);

  mdb_stat(txn.ptr(),
           container_.family(Family::ownershipSets),
           &stat);
  sets_size = stat_db_size(stat);

  mdb_stat(txn.ptr(),
           container_.family(Family::factOwners),
           &stat);
  num_owners = stat.ms_entries;
  owners_size = stat_db_size(stat);

  mdb_stat(txn.ptr(),
           container_.family(Family::factOwnerPages),
           &stat);
  num_owner_pages = stat.ms_entries;
  owner_pages_size = stat_db_size(stat);

  OwnershipStats stats;
  stats.num_units = nextUnitId() - first_unit_id; // accurate
  stats.units_size = units_size + unit_ids_size; // estimate
  stats.num_sets = next_uset_id - stats.num_units; // accurate
  stats.sets_size = sets_size; // estimate
  stats.num_owner_entries = num_owners + num_owner_pages; // estimate
  stats.owners_size = owners_size + owner_pages_size; // estimate

  auto orphans =
      readAdminValue<int64_t>(container_, AdminId::ORPHAN_FACTS);
  stats.num_orphan_facts = orphans ? *orphans : -1;

  return stats;
}

} // namespace impl
} // namespace lmdb
} // namespace glean
} // namespace facebook
