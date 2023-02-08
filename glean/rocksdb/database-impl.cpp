/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "glean/rocksdb/container-impl.h"
#include "glean/rocksdb/database-impl.h"

#include "glean/rts/timer.h"

namespace facebook {
namespace glean {
namespace rocks {
namespace impl {

using namespace rts;

namespace {

const char *admin_names[] = {
  "NEXT_ID",
  "VERSION",
  "STARTING_ID",
  "FIRST_UNIT_ID",
  "NEXT_UNIT_ID"
};

template <typename T, typename F>
T getAdminValue(
    ContainerImpl& container_,
    AdminId id,
    T def,
    bool write,
    F&& notFound) {
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
    notFound();
    if (write) {
      binary::Output value;
      value.fixed(def);
      check(container_.db->Put(
          container_.writeOptions,
          container_.family(Family::admin),
          slice(key),
          slice(value)));
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
    : container_(std::move(c)) {
  starting_id = Id::fromWord(getAdminValue(
      container_,
      AdminId::STARTING_ID,
      start.toWord(),
      container_.mode == Mode::Create,
      [] {}));

  next_id = Id::fromWord(getAdminValue(
      container_,
      AdminId::NEXT_ID,
      start.toWord(),
      container_.mode == Mode::Create,
      [mode = container_.mode] {
        if (mode != Mode::Create) {
          rts::error("corrupt database - missing NEXT_ID");
        }
      }));

  first_unit_id = getAdminValue(
      container_,
      AdminId::FIRST_UNIT_ID,
      first_unit_id_,
      container_.mode == Mode::Create,
      [] {
        // TODO: later this should be an error, for now we have to be
        // able to open old DBs.
      });
  VLOG(1) << folly::sformat("first_unit_id: {}", first_unit_id);

  next_uset_id = getAdminValue(
      container_,
      AdminId::NEXT_UNIT_ID,
      first_unit_id,
      container_.mode == Mode::Create,
      [] {
        // TODO: later this should be an error, for now we have to be
        // able to open old DBs.
      });
  VLOG(1) << folly::sformat("next_uset_id: {}", next_uset_id);

  db_version = getAdminValue(
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
  std::unique_ptr<rocksdb::Iterator> iter(container_.db->NewIterator(
      rocksdb::ReadOptions(), container_.family(Family::stats)));
  if (!iter) {
    rts::error("rocksdb: couldn't allocate iterator");
  }

  for (iter->SeekToFirst(); iter->Valid(); iter->Next()) {
    binary::Input key(byteRange(iter->key()));
    stats[key.fixed<Pid>()] = fromSlice<MemoryStats>(iter->value());
    assert(key.empty());
  }
  auto s = iter->status();
  if (!s.IsNotFound()) {
    check(s);
  }
  return stats;
}

Id DatabaseImpl::idByKey(Pid type, folly::ByteRange key) {
  if (count(type).high() == 0) {
    return Id::invalid();
  }

  container_.requireOpen();
  rocksdb::PinnableSlice out;
  binary::Output k;
  k.fixed(type);
  k.put(key);
  auto s = container_.db->Get(
      rocksdb::ReadOptions(), container_.family(Family::keys), slice(k), &out);
  if (s.IsNotFound()) {
    return Id::invalid();
  } else {
    check(s);
    binary::Input value = input(out);
    auto id = value.fixed<Id>();
    assert(value.empty());
    return id;
  }
}

Pid DatabaseImpl::typeById(Id id) {
  container_.requireOpen();
  rocksdb::PinnableSlice val;
  if (lookupById(id, val)) {
    return input(val).packed<Pid>();
  } else {
    return Pid::invalid();
  }
}

namespace {

rts::Fact::Ref decomposeFact(Id id, const rocksdb::Slice& data) {
  auto inp = input(data);
  const auto ty = inp.packed<Pid>();
  const auto key_size = inp.packed<uint32_t>();
  return rts::Fact::Ref{id, ty, rts::Fact::Clause::from(inp.bytes(), key_size)};
}

}

bool DatabaseImpl::factById(Id id, std::function<void(Pid, Fact::Clause)> f) {
  container_.requireOpen();
  rocksdb::PinnableSlice val;
  if (lookupById(id, val)) {
    auto ref = decomposeFact(id, val);
    f(ref.type, ref.clause);
    return true;
  } else {
    return false;
  }
}

bool DatabaseImpl::lookupById(Id id, rocksdb::PinnableSlice& val) const {
  if (id < startingId() || id >= firstFreeId()) {
    return false;
  }
  binary::Output key;
  key.nat(id.toWord());
  val.Reset();
  auto s = container_.db->Get(
      rocksdb::ReadOptions(),
      container_.family(Family::entities),
      slice(key),
      &val);
  if (s.IsNotFound()) {
    return false;
  } else {
    check(s);
    return true;
  }
}

namespace {

struct SeekIterator final : rts::FactIterator {
  SeekIterator(
      folly::ByteRange start,
      size_t prefix_size,
      Pid type,
      const DatabaseImpl* db)
      : upper_bound_(
            binary::lexicographicallyNext({start.data(), prefix_size})),
        upper_bound_slice_(
            reinterpret_cast<const char*>(upper_bound_.data()),
            upper_bound_.size()),
        type_(type),
        db_(db) {
    assert(prefix_size <= start.size());
    // both upper_bound_slice_ and options_ need to be alive for the duration
    // of the iteration
    options_.iterate_upper_bound = &upper_bound_slice_;
    iter_.reset(db->container_.db->NewIterator(
        options_, db->container_.family(Family::keys)));
    if (iter_) {
      iter_->Seek(slice(start));
    } else {
      rts::error("rocksdb: couldn't allocate iterator");
    }
  }

  void next() override {
    iter_->Next();
    auto s = iter_->status();
    if (!s.IsNotFound()) {
      check(s);
    }
  }

  Fact::Ref get(Demand demand) override {
    if (iter_->Valid()) {
      auto key = input(iter_->key());
      auto ty = key.fixed<Pid>();
      assert(ty == type_);
      auto value = input(iter_->value());
      auto id = value.fixed<Id>();
      assert(value.empty());

      if (demand == KeyOnly) {
        return Fact::Ref{id, type_, Fact::Clause::fromKey(key.bytes())};
      } else {
        auto found = db_->lookupById(id, slice_);
        assert(found);
        return decomposeFact(id, slice_);
      }
    } else {
      return Fact::Ref::invalid();
    }
  }

  std::optional<Id> lower_bound() override {
    return std::nullopt;
  }
  std::optional<Id> upper_bound() override {
    return std::nullopt;
  }

  const std::vector<unsigned char> upper_bound_;
  const rocksdb::Slice upper_bound_slice_;
  const Pid type_;
  rocksdb::ReadOptions options_;
  std::unique_ptr<rocksdb::Iterator> iter_;
  const DatabaseImpl* db_;
  rocksdb::PinnableSlice slice_;
};

}

std::unique_ptr<rts::FactIterator>
DatabaseImpl::seek(Pid type, folly::ByteRange start, size_t prefix_size) {
  assert(prefix_size <= start.size());
  if (count(type).high() == 0) {
    return std::make_unique<EmptyIterator>();
  }

  container_.requireOpen();
  binary::Output out;
  out.fixed(type);
  const auto type_size = out.size();
  out.put(start);
  return std::make_unique<SeekIterator>(
      out.bytes(), type_size + prefix_size, type, this);
}

std::unique_ptr<rts::FactIterator> DatabaseImpl::seekWithinSection(
    Pid type,
    folly::ByteRange start,
    size_t prefix_size,
    Id from,
    Id upto) {
  if (upto <= startingId() || firstFreeId() <= from) {
    return std::make_unique<EmptyIterator>();
  }

  return Section(this, from, upto).seek(type, start, prefix_size);
}

namespace {

template <typename Direction>
struct EnumerateIterator final : rts::FactIterator {
  static std::vector<char> encode(Id id) {
    std::vector<char> v(rts::MAX_NAT_SIZE);
    const auto n =
        rts::storeNat(reinterpret_cast<unsigned char*>(v.data()), id.toWord());
    v.resize(n);
    return v;
  }

  explicit EnumerateIterator(Id start, Id bound, const DatabaseImpl* db)
      : bound_(encode(bound)), bound_slice_(bound_.data(), bound_.size()) {
    // both the slice and options_ need to be alive for the duration
    // of the iteration
    options_.*Direction::iterate_bound = &bound_slice_;
    iter_.reset(db->container_.db->NewIterator(
        options_, db->container_.family(Family::entities)));

    auto st = encode(start);
    if (iter_) {
      (iter_.get()->*Direction::seek)({st.data(), st.size()});
    } else {
      rts::error("rocksdb: couldn't allocate iterator");
    }
  }

  void next() override {
    (iter_.get()->*Direction::next)();
    auto s = iter_->status();
    if (!s.IsNotFound()) {
      check(s);
    }
  }

  Fact::Ref get(Demand /*unused*/) override {
    return iter_->Valid()
        ? decomposeFact(
              Id::fromWord(
                  loadTrustedNat(reinterpret_cast<const unsigned char*>(
                                     iter_->key().data()))
                      .first),
              iter_->value())
        : Fact::Ref::invalid();
  }

  std::optional<Id> lower_bound() override {
    return std::nullopt;
  }
  std::optional<Id> upper_bound() override {
    return std::nullopt;
  }

  const std::vector<char> bound_;
  const rocksdb::Slice bound_slice_;
  rocksdb::ReadOptions options_;
  std::unique_ptr<rocksdb::Iterator> iter_;
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

  static inline constexpr auto iterate_bound =
      &rocksdb::ReadOptions::iterate_upper_bound;
  static inline constexpr auto seek = &rocksdb::Iterator::Seek;
  static inline constexpr auto next = &rocksdb::Iterator::Next;
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

  static inline constexpr auto iterate_bound =
      &rocksdb::ReadOptions::iterate_lower_bound;
  static inline constexpr auto seek = &rocksdb::Iterator::SeekForPrev;
  static inline constexpr auto next = &rocksdb::Iterator::Prev;
};

}

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

  rocksdb::WriteBatch batch;

  // NOTE: We do *not* support concurrent writes so we don't need to protect
  // stats_ here because nothing should be able to replace it while we're
  // running
  const auto& old_stats = stats_.unprotected();
  PredicateStats new_stats(old_stats);

  for (auto iter = facts.enumerate(); auto fact = iter->get(); iter->next()) {
    assert(fact.id >= next_id);

    uint64_t mem = 0;
    auto put = [&](auto family, const auto& key, const auto& value) {
      check(batch.Put(family, key, value));
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

      put(container_.family(Family::entities), slice(k), slice(v));
    }

    {
      binary::Output k;
      k.fixed(fact.type);
      k.put(fact.key());
      binary::Output v;
      v.fixed(fact.id);

      put(container_.family(Family::keys), slice(k), slice(v));
    }

    new_stats[fact.type] += MemoryStats::one(mem);
  }

  const auto first_free_id = facts.firstFreeId();
  check(batch.Put(
      container_.family(Family::admin),
      toSlice(AdminId::NEXT_ID),
      toSlice(first_free_id)));

  for (const auto& x : new_stats) {
    if (x.second != old_stats.get(x.first)) {
      check(batch.Put(
          container_.family(Family::stats),
          toSlice(x.first.toWord()),
          toSlice(x.second)));
    }
  }

  check(container_.db->Write(container_.writeOptions, &batch));
  next_id = first_free_id;

  stats_.set(std::move(new_stats));
}

}
}
}
} // namespace facebook
