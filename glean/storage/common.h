/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include <glean/rts/ownership/uset.h>
#include "glean/rts/binary.h"
#include "glean/rts/timer.h"

#include "glean/storage/db.h"

namespace facebook {
namespace glean {
namespace db {

using namespace rts;

enum class AdminId : uint32_t {
  NEXT_ID,
  VERSION,
  STARTING_ID,
  FIRST_UNIT_ID,
  NEXT_UNIT_ID,
  ORPHAN_FACTS,
};

template <typename T>
inline folly::ByteRange bytesOf(const T& x) {
  return folly::ByteRange(
      reinterpret_cast<const unsigned char*>(&x), sizeof(x));
}

template <typename T>
inline T fromBytes(const folly::ByteRange slice) {
  assert(slice.size() == sizeof(T));
  T x;
  std::memcpy(&x, slice.data(), slice.size());
  return x;
}

///
/// Common operations for DB implementations abstracted over
/// the ContainerImpl class, which must provide:
///
///   C::Family::<name>  - column families
///   C::Iterator read() - start reading
///   C::Writer write()  - start writing
///
///   typename C::Iterator (various operations)
///   typename C::Writer (various operations)
///
template <typename C>
struct DatabaseCommon : Database {
  DatabaseCommon(C c) : container_(std::move(c)) {};
  C container_;

  rts::UnitId first_unit_id;
  rts::UsetId next_uset_id; // also next UnitId, since they share a namespace
  std::vector<size_t> ownership_unit_counters;
  folly::F14FastMap<uint64_t, size_t> ownership_derived_counters;

  // Cached ownership sets, only used when writing.
  // Note: must only be accessed under the write lock
  std::unique_ptr<rts::Usets> usets_;

  std::vector<size_t> loadOwnershipUnitCounters();
  folly::F14FastMap<uint64_t, size_t> loadOwnershipDerivedCounters();

  std::unique_ptr<rts::OwnershipSetIterator> getSetIterator(
      DatabaseCommon<C>& db);

  std::unique_ptr<Usets> loadOwnershipSets();

  folly::Optional<uint32_t> getUnitId(folly::ByteRange unit) override;
  folly::Optional<std::string> getUnit(uint32_t unit_id) override;
  uint32_t nextUnitId();

  void addBatchDescriptor(BatchDescriptor batchDescriptor) override;
  void markBatchDescriptorAsWritten(folly::ByteRange location) override;
  bool isBatchDescriptorStored(folly::ByteRange location) override;
  std::vector<BatchDescriptor> getUnprocessedBatchDescriptors() override;

  void addOwnership(const std::vector<OwnershipSet>& ownership) override;

  std::unique_ptr<rts::DerivedFactOwnershipIterator>
  getDerivedFactOwnershipIterator(Pid pid) override;

  std::unique_ptr<rts::OwnershipUnitIterator> getOwnershipUnitIterator()
      override;

  void storeOwnership(ComputedOwnership& ownership) override;

  void addDefineOwnership(DefineOwnership& def) override;

  rts::UsetId getOwner(Id id) override;

  std::unique_ptr<rts::Ownership> getOwnership() override;

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
    static void prepare(C& container);
    void enable(C& container);

    // Lookup in the cache. Returns none if the cache is not enabled
    std::optional<rts::UsetId> getOwner(C& container, Id id);

   private:
    struct Page {
      std::vector<uint16_t> factIds;
      std::vector<rts::UsetId> setIds;
    };
    static rts::UsetId lookup(const Page& page, Id id);
    static std::unique_ptr<Page> readPage(C& container, uint64_t prefix);

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

  virtual OwnershipStats getOwnershipStats() = 0;
};

template <typename C>
std::vector<size_t> DatabaseCommon<C>::loadOwnershipUnitCounters() {
  container_.requireOpen();
  std::vector<size_t> result;

  auto iter = container_.read(C::Family::ownershipRaw);

  for (iter.seek_first(); iter.valid(); iter.next()) {
    binary::Input key(iter.key());
    auto id = key.trustedNat();
    if (id == first_unit_id + result.size()) {
      result.push_back(1);
    } else if (id + 1 == first_unit_id + result.size()) {
      ++result.back();
    } else {
      rts::error("rocksdb: invalid ownershipUnits {} {}", id, result.size());
    }
  }

  return result;
}

template <typename C>
folly::F14FastMap<uint64_t, size_t>
DatabaseCommon<C>::loadOwnershipDerivedCounters() {
  container_.requireOpen();
  folly::F14FastMap<uint64_t, size_t> result;

  auto iter = container_.read(C::Family::ownershipDerivedRaw);

  for (iter.seek_first(); iter.valid(); iter.next()) {
    binary::Input key(iter.key());
    auto pid = key.trustedNat();
    const auto [i, _] = result.insert({pid, 0});
    ++i->second;
  }

  VLOG(1) << "derived fact owners for " << result.size() << " pids";
  return result;
}

template <typename C>
void DatabaseCommon<C>::addBatchDescriptor(BatchDescriptor batchDescriptor) {
  container_.requireOpen();
  binary::Output value;
  value.nat(batchDescriptor.format);
  value.nat(0); // completed=false
  auto writer = container_.write();
  writer.put(
      C::Family::batchDescriptors,
      folly::ByteRange(
          reinterpret_cast<const unsigned char*>(
              batchDescriptor.location.data()),
          batchDescriptor.location.size()),
      value.bytes());
  writer.commit();
}

template <typename C>
void DatabaseCommon<C>::markBatchDescriptorAsWritten(
    folly::ByteRange location) {
  container_.requireOpen();
  uint32_t format = 0;
  bool found = container_.get(
      C::Family::batchDescriptors, location, [&format](folly::ByteRange val) {
        binary::Input input(val);
        format = static_cast<uint32_t>(input.trustedNat());
      });
  if (!found) {
    LOG(WARNING) << "batch descriptor not found for location "
                 << folly::hexlify(location);
    return;
  }
  binary::Output value;
  value.nat(format);
  value.nat(1); // completed=true
  auto writer = container_.write();
  writer.put(C::Family::batchDescriptors, location, value.bytes());
  writer.commit();
}

template <typename C>
bool DatabaseCommon<C>::isBatchDescriptorStored(folly::ByteRange location) {
  container_.requireOpen();
  return container_.get(
      C::Family::batchDescriptors, location, [](folly::ByteRange) {});
}

template <typename C>
std::vector<Database::BatchDescriptor>
DatabaseCommon<C>::getUnprocessedBatchDescriptors() {
  container_.requireOpen();
  std::vector<Database::BatchDescriptor> result;
  size_t total = 0;
  auto iter = container_.read(C::Family::batchDescriptors);
  for (iter.seek_first(); iter.valid(); iter.next()) {
    ++total;
    auto key = iter.key();
    auto val = iter.value();
    binary::Input input(val);
    uint32_t format = static_cast<uint32_t>(input.trustedNat());
    bool completed = input.trustedNat() != 0;
    if (!completed) {
      Database::BatchDescriptor entry;
      entry.location =
          std::string(reinterpret_cast<const char*>(key.data()), key.size());
      entry.format = format;
      result.push_back(std::move(entry));
    }
  }
  LOG(INFO) << "batch descriptors loaded: " << total << " total, "
            << result.size() << " unprocessed";
  return result;
}

template <typename C>
std::unique_ptr<rts::OwnershipSetIterator> DatabaseCommon<C>::getSetIterator(
    DatabaseCommon<C>& db) {
  struct SetIterator : rts::OwnershipSetIterator {
    explicit SetIterator(DatabaseCommon<C>& db)
        : iter(db.container_.read(C::Family::ownershipSets)) {
      size_t last;

      iter.seek_last();
      if (!iter.valid()) {
        last = 0;
      } else {
        binary::Input key(iter.key());
        last = key.trustedNat();
      }

      iter.seek_first();
      if (!iter.valid()) {
        first_ = db.next_uset_id;
        size_ = 0;
      } else {
        binary::Input key(iter.key());
        first_ = key.trustedNat();
        size_ = last - first_ + 1;
      }
    }

    folly::Optional<std::pair<UsetId, SetExpr<const OwnerSet*>>> get()
        override {
      if (!initial) {
        iter.next();
      } else {
        initial = false;
      }
      if (iter.valid()) {
        binary::Input key(iter.key());
        auto usetid = key.trustedNat();
        // EliasFano needs to be able to read 8 bytes past the end of
        // the data, so we have to copy the bytes to add padding.
        const size_t pad = 8;
        bytes = hs::ffi::clone_array<uint8_t>(
            reinterpret_cast<const uint8_t*>(iter.value().data()),
            iter.value().size(),
            pad);
        binary::Input val(bytes.get(), iter.value().size());
        exp.op = static_cast<SetOp>(val.trustedNat());
        exp.set = deserializeEliasFano(val);
        return std::pair<uint32_t, SetExpr<const OwnerSet*>>(
            usetid, {exp.op, &exp.set});
      } else {
        return folly::none;
      }
    }

    std::pair<size_t, size_t> sizes() const override {
      return {first_, size_};
    }

    hs::ffi::malloced_array<uint8_t> bytes;
    bool initial = true;
    SetExpr<OwnerSet> exp;
    size_t first_, size_;
    typename C::Iterator iter;
  };

  return std::make_unique<SetIterator>(db);
}

template <typename C>
std::unique_ptr<Usets> DatabaseCommon<C>::loadOwnershipSets() {
  auto t = makeAutoTimer("loadOwnershipSets");

  auto iter = getSetIterator(*this);
  auto pair = iter->sizes();
  auto first = pair.first;
  auto size = pair.second;

  auto usets = std::make_unique<Usets>(first + size);

  while (const auto iterPair = iter->get()) {
    auto set = SetU32::fromEliasFano(*iterPair->second.set);
    // paranoia: check the set contents make sense
    set.foreach([first, size](UsetId id) {
      if (id >= first + size) {
        rts::error(
            "invalid ownershipSets: id out of range: {} {} {}",
            id,
            first,
            size);
      }
    });
    auto p = usets->add(std::move(set), 0);
    p->id = iterPair->first;
  }
  auto stats = usets->statistics();

  LOG(INFO) << "loadOwnershipSets loaded " << stats.adds << " sets, "
            << stats.bytes << " bytes";

  return usets;
}

template <typename C>
folly::Optional<uint32_t> DatabaseCommon<C>::getUnitId(folly::ByteRange unit) {
  uint32_t r;
  if (container_.get(C::Family::ownershipUnits, unit, [&](auto val) {
        assert(val.size() == sizeof(uint32_t));
        r = folly::loadUnaligned<uint32_t>(val.data());
      })) {
    return r;
  } else {
    return folly::none;
  }
}

template <typename C>
uint32_t DatabaseCommon<C>::nextUnitId() {
  auto iter = container_.read(C::Family::ownershipUnitIds);

  size_t max_unit_id;

  iter.seek_last();
  if (!iter.valid()) {
    max_unit_id = first_unit_id;
  } else {
    binary::Input key(iter.key());
    max_unit_id = key.trustedNat();
  }

  return max_unit_id;
}

template <typename C>
folly::Optional<std::string> DatabaseCommon<C>::getUnit(uint32_t unit_id) {
  std::string r;
  EncodedNat key(unit_id);
  if (container_.get(
          C::Family::ownershipUnitIds, key.byteRange(), [&](auto val) {
            r = binary::mkString(val);
          })) {
    return r;
  } else {
    return folly::none;
  }
}

// Called once per batch inside Store.commit.
// Only function that can add new UnitIds to the DB.
// Adds to
//   - Family::ownershipUnits
//   - Family::ownershipUnitIds
//   - Family::ownershipRaw
//   - Family::admin
template <typename C>
void DatabaseCommon<C>::addOwnership(
    const std::vector<OwnershipSet>& ownership) {
  container_.requireOpen();

  if (ownership.empty()) {
    return;
  }

  size_t new_count = 0;
  std::vector<size_t> touched;

  auto writer = container_.write();

  for (const auto& set : ownership) {
    uint32_t unit_id;
    auto res = getUnitId(set.unit);
    if (res.hasValue()) {
      unit_id = *res;
      if (unit_id >= first_unit_id + ownership_unit_counters.size()) {
        rts::error("inconsistent unit id {}", unit_id);
      }
      touched.push_back(unit_id);
    } else {
      unit_id = first_unit_id + ownership_unit_counters.size() + new_count;
      writer.put(C::Family::ownershipUnits, set.unit, bytesOf(unit_id));
      EncodedNat key(unit_id);
      writer.put(C::Family::ownershipUnitIds, key.byteRange(), set.unit);
      ++new_count;
    }

    binary::Output key;
    key.nat(unit_id);
    key.nat(
        unit_id < first_unit_id + ownership_unit_counters.size()
            ? ownership_unit_counters[unit_id - first_unit_id]
            : 0);
    folly::ByteRange val(
        reinterpret_cast<const unsigned char*>(set.ids.data()),
        set.ids.size() * sizeof(int64_t));
    writer.put(C::Family::ownershipRaw, key.bytes(), val);
  }

  if (new_count > 0) {
    next_uset_id += new_count;

    writer.put(
        C::Family::admin,
        bytesOf(AdminId::NEXT_UNIT_ID),
        bytesOf(next_uset_id));
  }

  writer.commit();

  for (auto i : touched) {
    CHECK_LT(i, first_unit_id + ownership_unit_counters.size());
    ++ownership_unit_counters[i - first_unit_id];
  }
  ownership_unit_counters.insert(ownership_unit_counters.end(), new_count, 1);
  CHECK_EQ(next_uset_id, ownership_unit_counters.size() + first_unit_id);
}

template <typename C>
std::unique_ptr<rts::DerivedFactOwnershipIterator>
DatabaseCommon<C>::getDerivedFactOwnershipIterator(Pid pid) {
  struct DerivedFactIterator : rts::DerivedFactOwnershipIterator {
    explicit DerivedFactIterator(Pid pid, C& container_)
        : pid_(pid), iter(container_.read(C::Family::ownershipDerivedRaw)) {
      EncodedNat key(pid.toWord());
      iter.seek_key(key.byteRange());
    }

    folly::Optional<DerivedFactOwnership> get() override {
      if (iter.valid()) {
        binary::Input key(iter.key());
        auto pid = key.trustedNat();
        if (pid != pid_.toWord()) {
          return {};
        }
        const auto val = iter.value();
        const size_t elts = val.size() / (sizeof(uint32_t) + sizeof(uint64_t));
        const Id* ids = reinterpret_cast<const Id*>(val.data());
        const UsetId* owners = reinterpret_cast<const UsetId*>(
            val.data() + elts * sizeof(uint64_t));
        iter.next();
        return rts::DerivedFactOwnership{{ids, elts}, {owners, elts}};
      } else {
        return folly::none;
      }
    }

    Pid pid_;
    typename C::Iterator iter;
  };

  return std::make_unique<DerivedFactIterator>(pid, container_);
}

template <typename C>
std::unique_ptr<rts::OwnershipUnitIterator>
DatabaseCommon<C>::getOwnershipUnitIterator() {
  struct UnitIterator : rts::OwnershipUnitIterator {
    explicit UnitIterator(C& container_)
        : iter(container_.read(C::Family::ownershipRaw)) {
      iter.seek_first();
    }

    folly::Optional<rts::OwnershipUnit> get() override {
      if (iter.valid()) {
        binary::Input key(iter.key());
        auto unit = key.trustedNat();
        const auto val = iter.value();
        iter.next();
        return rts::OwnershipUnit{
            static_cast<uint32_t>(unit),
            {reinterpret_cast<const OwnershipUnit::Ids*>(val.data()),
             val.size() / sizeof(OwnershipUnit::Ids)}};
      } else {
        return {};
      }
    }

    typename C::Iterator iter;
  };

  return std::make_unique<UnitIterator>(container_);
}

namespace {

template <typename C>
void putOwnerSet(
    C& container,
    typename C::Writer& writer,
    UsetId id,
    SetOp op,
    const OwnerSet& set) {
  binary::Output key;
  key.nat(id);
  binary::Output value;
  value.nat(op);
  serializeEliasFano(value, set);
  writer.put(C::Family::ownershipSets, key.bytes(), value.bytes());
}

} // namespace

template <typename C>
void DatabaseCommon<C>::storeOwnership(ComputedOwnership& ownership) {
  container_.requireOpen();

  if (ownership.sets_.size() > 0) {
    auto t = makeAutoTimer("storeOwnership(sets)");
    typename C::Writer writer = container_.write();

    auto upper = ownership.sets_.getFirstId() + ownership.sets_.size();

    auto serialized = ownership.sets_.toEliasFano(upper);
    uint32_t id = ownership.sets_.getFirstId();
    CHECK_GE(id, next_uset_id);

    for (auto& exp : serialized) {
      if ((id % 1000000) == 0) {
        VLOG(1) << "storeOwnership: " << id;
      }
      putOwnerSet(container_, writer, id, exp.op, exp.set);
      exp.set.free();
      id++;
    }

    next_uset_id = upper;
    writer.put(
        C::Family::admin,
        bytesOf(AdminId::NEXT_UNIT_ID),
        bytesOf(next_uset_id));

    VLOG(1) << "storeOwnership: writing sets (" << ownership.sets_.size()
            << ")";
    writer.commit();

    if (usets_->size() == 0) {
      // If usets_ is empty, then it will not have the correct firstId yet
      usets_ = std::make_unique<Usets>(ownership.sets_.getFirstId());
    }
    usets_->append(std::move(ownership.sets_));
  }

  CHECK(usets_->size() == 0 || usets_->getNextId() == next_uset_id);
  // TODO: better not add new units after storing sets, we should fail if that
  // happens

  if (ownership.facts_.size() > 0) {
    auto t = makeAutoTimer("storeOwnership(facts)");

    auto hasOwner = [&](EncodedNat& key) -> bool {
      bool r = false;
      container_.get(
          C::Family::factOwners, key.byteRange(), [&](auto) { r = true; });
      return r;
    };

    typename C::Writer writer = container_.write();
    for (uint64_t i = 0; i < ownership.facts_.size(); i++) {
      auto id = ownership.facts_[i].first;
      auto usetid = ownership.facts_[i].second;
      EncodedNat key(id.toWord());
      if (usetid != INVALID_USET || !hasOwner(key)) {
        if (usetid != INVALID_USET) {
          CHECK_LT(usetid, next_uset_id);
        }
        // This is an interval map, and we might be writing multiple sparse sets
        // of intervals where the gaps are indicated by INVALID_USET. Therefore
        // don't overwrite an existing owner with INVALID_USET.
        EncodedNat val(usetid);
        writer.put(C::Family::factOwners, key.byteRange(), val.byteRange());
      }
    }
    VLOG(1) << "storeOwnership: writing facts: " << ownership.facts_.size()
            << " intervals";
    writer.commit();
  }
}

// Called once per batch. Can't be run in parallel.
// Will add data into:
// - Family::ownershipSets
// - Family::ownershipDerivedRaw
// - Family::admin
template <typename C>
void DatabaseCommon<C>::addDefineOwnership(DefineOwnership& def) {
  auto t = makeAutoTimer("addDefineOwnership");
  container_.requireOpen();

  VLOG(1) << "addDefineOwnership: " << def.usets_.size() << " sets";

  // add new owner sets
  if (def.newSets_.size() > 0) {
    folly::F14FastMap<UsetId, UsetId> substitution;
    auto subst = [&](uint32_t old) -> uint32_t {
      auto n = substitution.find(old);
      if (n == substitution.end()) {
        return old;
      } else {
        return n->second;
      }
    };

    typename C::Writer writer = container_.write();
    size_t numNewSets = 0;

    for (auto uset : def.newSets_) {
      std::set<UsetId> s;
      uset->exp.set.foreach([&](uint32_t elt) {
        UsetId newelt = subst(elt);
        if (newelt >= next_uset_id) {
          rts::error(
              "set id out of range: {} {} {}", newelt, next_uset_id, elt);
        }
        s.insert(newelt);
      });
      SetU32 set = SetU32::from(s);

      auto newUset = std::make_unique<Uset>(std::move(set), uset->exp.op, 0);
      auto p = newUset.get();
      auto oldId = uset->id;
      auto q = usets_->add(std::move(newUset));
      if (p == q) {
        usets_->promote(p);
        assert(next_uset_id == p->id);
        next_uset_id++;
        auto ownerset = p->toEliasFano(next_uset_id);
        putOwnerSet(container_, writer, p->id, ownerset.op, ownerset.set);
        ownerset.set.free();
        numNewSets++;
      }
      VLOG(2) << "rebased set " << oldId << " -> " << q->id;
      substitution[oldId] = q->id;
    }

    if (numNewSets > 0) {
      writer.put(
          C::Family::admin,
          bytesOf(AdminId::NEXT_UNIT_ID),
          bytesOf(next_uset_id));
    }

    VLOG(1) << "addDefineOwnership: writing sets (" << numNewSets << ")";
    writer.commit();

    for (auto& [_, pred] : def.defines_) {
      for (auto& owner : pred.owners_) {
        owner = subst(owner);
      }

      for (auto& owner : pred.new_owners_) {
        owner = subst(owner);
      }
    }
  }

  typename C::Writer writer = container_.write();

  for (const auto& [pid, pred] : def) {
    VLOG(1) << "addDefineOwnership: "
            << pred.owners_.size() + pred.new_owners_.size()
            << " owners, for pid " << pid.toWord();

    // ownershipDerivedRaw :: (Pid,nat) -> vector<int64_t>
    //
    // Similarly to ownershipRaw, this is basically just an
    // append-only log. The nat in the key is a per-Pid counter that
    // we bump by one each time we add another batch of data for a
    // Pid.

    binary::Output key;
    key.nat(pid.toWord());
    const auto [it, _] = ownership_derived_counters.insert({pid.toWord(), 0});
    key.nat(it->second++);

    binary::Output val;

    val.bytes(
        pred.ids_.data(),
        pred.ids_.size() *
            sizeof(typename std::remove_reference<
                   decltype(pred.ids_)>::type::value_type));
    val.bytes(
        pred.new_ids_.data(),
        pred.new_ids_.size() *
            sizeof(typename std::remove_reference<
                   decltype(pred.new_ids_)>::type::value_type));
    val.bytes(
        pred.owners_.data(),
        pred.owners_.size() *
            sizeof(typename std::remove_reference<
                   decltype(pred.owners_)>::type::value_type));
    val.bytes(
        pred.new_owners_.data(),
        pred.new_owners_.size() *
            sizeof(typename std::remove_reference<
                   decltype(pred.new_owners_)>::type::value_type));

    writer.put(C::Family::ownershipDerivedRaw, key.bytes(), val.bytes());

    VLOG(1) << "addDefineOwnership wrote "
            << pred.ids_.size() + pred.new_ids_.size() << " entries for pid "
            << pid.toWord();
  }

  writer.commit();
}

template <typename C>
void DatabaseCommon<C>::cacheOwnership() {
  factOwnerCache_.enable(container_);
}

template <typename C>
void DatabaseCommon<C>::prepareFactOwnerCache() {
  FactOwnerCache::prepare(container_);
}

template <typename C>
UsetId DatabaseCommon<C>::getOwner(Id id) {
  auto cached = factOwnerCache_.getOwner(container_, id);

  if (cached) {
    return cached.value();
  } else {
    // cache is not enabled; fall back to reading from the DB.
    auto iter = container_.read(C::Family::factOwners);

    EncodedNat key(id.toWord());
    iter.seek_key_lower(key.byteRange());
    if (iter.valid()) {
      binary::Input val(iter.value());
      return val.trustedNat();
    } else {
      return INVALID_USET;
    }
  }
}

namespace {
// key in factOwnerPages where we store the index
static const std::string INDEX_KEY = "INDEX";

// sentinel value in the index indicating that this prefix has a page in
// factOwnerPages
static const UsetId HAS_PAGE = SPECIAL_USET;

// Note: this constant affects the data in the DB, so it can't be changed.
static const size_t PAGE_BITS = 12;
static const uint64_t PAGE_MASK = (1 << PAGE_BITS) - 1;
} // namespace

template <typename C>
void DatabaseCommon<C>::FactOwnerCache::enable(C& container) {
  auto cache = cache_.ulock();
  if (*cache) {
    return;
  }

  std::vector<UsetId> index;

  if (!container.get(
          C::Family::factOwnerPages,
          binary::byteRange(INDEX_KEY),
          [&](auto val) {
            CHECK_EQ(val.size() % sizeof(UsetId), 0);
            size_t num = val.size() / sizeof(UsetId);
            index.resize(num);
            const UsetId* start = reinterpret_cast<const UsetId*>(val.data());
            std::copy(start, start + num, index.data());
            VLOG(1) << folly::sformat("owner cache index: {} entries", num);
          })) {
    LOG(WARNING) << "cannot enable cache; missing INDEX";
    // assume this is an old DB without factOwnerPages, we'll fall back
    // to using factOwners.
    return;
  }

  size_t size = index.size() * sizeof(UsetId);
  Cache content{
      .index = std::move(index),
      .pages = {},
      .size_ = size,
  };

  auto wcache = cache.moveFromUpgradeToWrite();
  *wcache = std::make_unique<Cache>(std::move(content));
}

template <typename C>
std::unique_ptr<typename DatabaseCommon<C>::FactOwnerCache::Page>
DatabaseCommon<C>::FactOwnerCache::readPage(C& container, uint64_t prefix) {
  auto p = std::make_unique<FactOwnerCache::Page>();

  if (!container.get(C::Family::factOwnerPages, bytesOf(prefix), [&](auto val) {
        size_t num = val.size() / (sizeof(int16_t) + sizeof(UsetId));
        p->factIds.resize(num);
        p->setIds.resize(num);
        const uint16_t* ids = reinterpret_cast<const uint16_t*>(val.data());
        const UsetId* sets = reinterpret_cast<const UsetId*>(
            reinterpret_cast<const uint8_t*>(val.data()) +
            num * sizeof(uint16_t));
        std::copy(ids, ids + num, p->factIds.data());
        std::copy(sets, sets + num, p->setIds.data());
      })) {
    rts::error("missing page: {}", prefix);
  }

  return p;
}

template <typename C>
UsetId DatabaseCommon<C>::FactOwnerCache::lookup(
    const DatabaseCommon<C>::FactOwnerCache::Page& page,
    Id id) {
  // next binary-search on the content of the page to find the
  // interval containing the desired fact ID.
  uint32_t low, high, mid;
  uint16_t ix = id.toWord() & PAGE_MASK;

  low = 0;
  high = page.factIds.size();

  if (high == 0) {
    rts::error("empty page");
  }

  // low is inclusive, high is exclusive
  // low..high always contains an element that is <= id, if there is one
  while (high - low > 1) {
    mid = (high + low) / 2;
    auto x = page.factIds[mid];
    if (x == ix) {
      return page.setIds[mid];
    }
    if (x < ix) {
      low = mid;
    } else {
      high = mid;
    }
  }

  if (page.factIds[low] <= ix) {
    return page.setIds[low];
  } else {
    rts::error(
        "missing lower bound, ix={} low={} prefix={}",
        ix,
        low,
        id.toWord() >> PAGE_BITS);
  }
}

template <typename C>
std::optional<UsetId> DatabaseCommon<C>::FactOwnerCache::getOwner(
    C& container,
    Id id) {
  auto cachePtr = cache_.rlock();
  auto cache = cachePtr->get();

  if (!cache) {
    return {};
  }

  // first find the right page
  auto prefix = id.toWord() >> PAGE_BITS;

  if (prefix >= cache->index.size()) {
    // The DB may have no or partial ownership information, leading to a
    // smaller than expected index, in which case the missing entries
    // are facts with no ownership.
    return INVALID_USET;
  }
  UsetId pageval = cache->index[prefix];
  if (pageval != HAS_PAGE) {
    return pageval;
  }

  const FactOwnerCache::Page* page;

  // grab the Page from the cache, or read it from the DB
  if (prefix < cache->pages.size() && cache->pages[prefix]) {
    page = cache->pages[prefix].get();
    cachePtr.unlock();
  } else {
    cachePtr.unlock();

    auto p = FactOwnerCache::readPage(container, prefix);
    auto wlock = cache_.wlock();
    auto wcache = wlock->get();
    auto size = wcache->size_;

    if (prefix >= wcache->pages.size()) {
      wcache->pages.resize(prefix + 1);
    }
    if (!wcache->pages[prefix]) {
      wcache->size_ += p->factIds.size() * sizeof(uint16_t) +
          p->setIds.size() * sizeof(UsetId) + sizeof(*p);
      wcache->pages[prefix] = std::move(p);
    }

    VLOG(2) << "new page(" << prefix
            << ") size = " << folly::prettyPrint(size, folly::PRETTY_BYTES_IEC);
    page = wcache->pages[prefix].get();
  }

  return FactOwnerCache::lookup(*page, id);
}

template <typename C>
void DatabaseCommon<C>::FactOwnerCache::prepare(C& container) {
  auto t = makeAutoTimer("prepareFactOwnerCache");

  auto writer = container.write();
  auto iter = container.read(C::Family::factOwners);

  iter.seek_first();

  std::vector<UsetId> index; // indexed by prefix
  uint64_t prefix = 0; // prefix of the current page
  UsetId set = INVALID_USET; // always the last set we saw
  std::vector<uint16_t> ids; // in the current page
  std::vector<UsetId> sets; // in the current page
  size_t populated = 0; // for stats
  int64_t orphaned = 0; // counts the orphan facts

  auto writePage = [&]() {
    if (ids.size() == 0) {
      index.push_back(set);
    } else {
      index.push_back(HAS_PAGE);
      binary::Output out;
      out.bytes(ids.data(), ids.size() * sizeof(uint16_t));
      out.bytes(sets.data(), sets.size() * sizeof(UsetId));
      writer.put(C::Family::factOwnerPages, bytesOf(prefix), out.bytes());
      ids.clear();
      sets.clear();
      populated++;
    };
  };

  uint64_t prev = Id::lowest().toWord();
  for (; iter.valid(); iter.next()) {
    binary::Input key(iter.key());
    uint64_t id = key.trustedNat();

    uint64_t this_prefix = id >> PAGE_BITS;
    uint16_t this_offset = id & PAGE_MASK;

    if (this_prefix != prefix) {
      for (; prefix < this_prefix; prefix++) {
        writePage();
      }
    }
    if (this_offset != 0 && ids.size() == 0) {
      // fill in the lower bound with the previous set
      ids.push_back(0);
      sets.push_back(set);
    }
    if (set == INVALID_USET && id > prev) {
      // track the number of orphaned facts
      orphaned += id - prev;
      VLOG(2) << folly::sformat("orphaned fact(s) {}-{}", prev, id - 1);
    }

    binary::Input val(iter.value());
    set = val.trustedNat();
    ids.push_back(this_offset);
    sets.push_back(set);

    prev = id;
  }

  // write the last page
  writePage();

  writer.put(
      C::Family::factOwnerPages,
      binary::byteRange(INDEX_KEY),
      folly::ByteRange(
          reinterpret_cast<const uint8_t*>(index.data()),
          index.size() * sizeof(UsetId)));

  t.logFormat(
      "{} index entries, {} populated, {} orphans",
      index.size(),
      populated,
      orphaned);

  // record the number of orphaned facts, this will be fetched by ownershipStats
  writer.put(
      C::Family::admin, bytesOf(AdminId::ORPHAN_FACTS), bytesOf(orphaned));

  writer.commit();
}

namespace {

//
// Wrapper around DatabaseImpl. Doesn't hold any extra data of its own.
//
template <typename C>
struct StoredOwnership : Ownership {
  explicit StoredOwnership(DatabaseCommon<C>* db) : db_(db) {}

  UsetId nextSetId() override {
    return db_->next_uset_id;
  }

  UsetId lookupSet(Uset* uset) override {
    if (!db_->usets_) {
      rts::error("rocksdb: lookupSet on read-only DB");
    } else {
      auto existing = db_->usets_->lookup(uset);
      if (existing) {
        return existing->id;
      } else {
        return INVALID_USET;
      }
    }
  }

  folly::Optional<SetExpr<SetU32>> getUset(UsetId id) override {
    EncodedNat key(id);
    folly::Optional<SetExpr<SetU32>> r;
    if (db_->container_.get(
            C::Family::ownershipSets, key.byteRange(), [&](auto val) {
              binary::Input inp(val);
              SetExpr<SetU32> exp;
              exp.op = static_cast<SetOp>(inp.trustedNat());
              exp.set = SetU32::fromEliasFano(deserializeEliasFano(inp));
              r = exp;
            })) {
      return r;
    } else {
      return folly::none;
    }
  }

  std::unique_ptr<rts::OwnershipSetIterator> getSetIterator() override {
    return db_->getSetIterator(*db_);
  }

  folly::Optional<UnitId> getUnitId(folly::ByteRange unit) override {
    return db_->getUnitId(unit);
  }

  OwnershipStats getStats() override {
    return db_->getOwnershipStats();
  }

 private:
  DatabaseCommon<C>* db_;
};

} // namespace

template <typename C>
std::unique_ptr<rts::Ownership> DatabaseCommon<C>::getOwnership() {
  container_.requireOpen();
  return std::make_unique<StoredOwnership<C>>(this);
}
} // namespace db
} // namespace glean
} // namespace facebook
