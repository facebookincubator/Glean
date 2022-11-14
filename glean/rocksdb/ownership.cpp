/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "glean/rocksdb/database-impl.h"
#include "glean/rts/timer.h"

namespace facebook {
namespace glean {
namespace rocks {
namespace impl {

using namespace rts;

std::vector<size_t> DatabaseImpl::loadOwnershipUnitCounters() {
  container_.requireOpen();
  std::vector<size_t> result;

  std::unique_ptr<rocksdb::Iterator> iter(container_.db->NewIterator(
      rocksdb::ReadOptions(), container_.family(Family::ownershipRaw)));

  if (!iter) {
    rts::error("rocksdb: couldn't allocate iterator");
  }

  for (iter->SeekToFirst(); iter->Valid(); iter->Next()) {
    binary::Input key(byteRange(iter->key()));
    auto id = key.trustedNat();
    if (id == result.size()) {
      result.push_back(0);
    } else if (id + 1 == result.size()) {
      ++result.back();
    } else {
      rts::error("rocksdb: invalid ownershipUnits");
    }
  }

  return result;
}

folly::F14FastMap<uint64_t, size_t>
DatabaseImpl::loadOwnershipDerivedCounters() {
  container_.requireOpen();
  folly::F14FastMap<uint64_t, size_t> result;

  std::unique_ptr<rocksdb::Iterator> iter(container_.db->NewIterator(
      rocksdb::ReadOptions(), container_.family(Family::ownershipDerivedRaw)));

  if (!iter) {
    rts::error("rocksdb: couldn't allocate iterator");
  }

  for (iter->SeekToFirst(); iter->Valid(); iter->Next()) {
    binary::Input key(byteRange(iter->key()));
    auto pid = key.trustedNat();
    const auto [i, _] = result.insert({pid, 0});
    ++i->second;
  }

  VLOG(1) << "derived fact owners for " << result.size() << " pids";
  return result;
}

namespace {

void serializeEliasFano(binary::Output& out, const OwnerSet& set) {
  out.nat(set.size);
  out.nat(set.numLowerBits);
  out.nat(set.upperSizeBytes);
  out.nat(set.skipPointers - set.data.begin());
  out.nat(set.forwardPointers - set.data.begin());
  out.nat(set.lower - set.data.begin());
  out.nat(set.upper - set.data.begin());
  out.put(set.data);
}

void deserializeEliasFano(binary::Input& in, OwnerSet& set) {
  set.size = in.trustedNat();
  set.numLowerBits = in.trustedNat();
  set.upperSizeBytes = in.trustedNat();
  auto skipPointers = in.trustedNat();
  auto forwardPointers = in.trustedNat();
  auto lower = in.trustedNat();
  auto upper = in.trustedNat();
  set.data = in.bytes();
  set.skipPointers = set.data.begin() + skipPointers;
  set.forwardPointers = set.data.begin() + forwardPointers;
  set.lower = set.data.begin() + lower;
  set.upper = set.data.begin() + upper;
}

void putOwnerSet(
    ContainerImpl& container,
    rocksdb::WriteBatch& batch,
    UsetId id,
    SetOp op,
    const OwnerSet& set) {
  binary::Output key;
  key.nat(id);
  binary::Output value;
  value.nat(op);
  serializeEliasFano(value, set);
  check(batch.Put(
      container.family(Family::ownershipSets), slice(key), slice(value)));
}

std::unique_ptr<rts::OwnershipSetIterator> getSetIterator(ContainerImpl& container) {
  struct SetIterator : rts::OwnershipSetIterator {
    explicit SetIterator(
        size_t first,
        size_t size,
        std::unique_ptr<rocksdb::Iterator> i)
        : first_(first), size_(size), iter(std::move(i)) {}

    folly::Optional<std::pair<UsetId, SetExpr<const OwnerSet*>>> get()
        override {
      if (iter->Valid()) {
        binary::Input key(byteRange(iter->key()));
        auto usetid = key.trustedNat();
        binary::Input val(byteRange(iter->value()));
        iter->Next();
        exp.op = static_cast<SetOp>(val.trustedNat());
        deserializeEliasFano(val, exp.set);
        return std::pair<uint32_t, SetExpr<const OwnerSet*>>(
            usetid, {exp.op, &exp.set});
      } else {
        return folly::none;
      }
    }

    std::pair<size_t, size_t> sizes() const override {
      return {first_, size_};
    }

    SetExpr<OwnerSet> exp;
    size_t first_, size_;
    std::unique_ptr<rocksdb::Iterator> iter;
  };

  std::unique_ptr<rocksdb::Iterator> iter(container.db->NewIterator(
      rocksdb::ReadOptions(), container.family(Family::ownershipSets)));

  if (!iter) {
    rts::error("rocksdb: couldn't allocate ownership set iterator");
  }

  size_t first, last, size;

  iter->SeekToLast();
  if (!iter->Valid()) {
    last = 0;
  } else {
    binary::Input key(byteRange(iter->key()));
    last = key.trustedNat();
  }

  iter->SeekToFirst();
  if (!iter->Valid()) {
    first = 0;
    size = 0;
  } else {
    binary::Input key(byteRange(iter->key()));
    first = key.trustedNat();
    size = last - first + 1;
  }

  return std::make_unique<SetIterator>(first, size, std::move(iter));
}

}

std::unique_ptr<Usets> DatabaseImpl::loadOwnershipSets() {
  auto t = makeAutoTimer("loadOwnershipSets");

  auto iter = getSetIterator(container_);
  auto [first, size] = iter->sizes();

  auto usets = std::make_unique<Usets>(first + size);

  while (const auto pair = iter->get()) {
    auto set = SetU32::fromEliasFano(*pair->second.set);
    auto p = usets->add(std::move(set), 0);
    p->id = pair->first;
  }
  auto stats = usets->statistics();

  LOG(INFO) << "loadOwnershipSets loaded " << stats.adds << " sets, "
            << stats.bytes << " bytes";

  return usets;
}

folly::Optional<uint32_t> DatabaseImpl::getUnitId(folly::ByteRange unit) {
  rocksdb::PinnableSlice val;
  auto s = container_.db->Get(
      rocksdb::ReadOptions(),
      container_.family(Family::ownershipUnits),
      slice(unit),
      &val);
  if (!s.IsNotFound()) {
    check(s);
    assert(val.size() == sizeof(uint32_t));
    return folly::loadUnaligned<uint32_t>(val.data());
  } else {
    return folly::none;
  }
}

folly::Optional<std::string> DatabaseImpl::getUnit(uint32_t unit_id) {
  rocksdb::PinnableSlice val;
  auto s = container_.db->Get(
      rocksdb::ReadOptions(),
      container_.family(Family::ownershipUnitIds),
      slice(EncodedNat(unit_id).byteRange()),
      &val);
  if (!s.IsNotFound()) {
    check(s);
    return val.ToString();
  } else {
    return folly::none;
  }
}

void DatabaseImpl::addOwnership(const std::vector<OwnershipSet>& ownership) {
  container_.requireOpen();

  if (ownership.empty()) {
    return;
  }

  size_t new_count = 0;
  std::vector<size_t> touched;
  rocksdb::WriteBatch batch;

  for (const auto& set : ownership) {
    uint32_t unit_id;
    auto res = getUnitId(set.unit);
    if (res.hasValue()) {
      unit_id = *res;
      if (unit_id >= ownership_unit_counters.size()) {
        rts::error("inconsistent unit id {}", unit_id);
      }
      touched.push_back(unit_id);
    } else {
      unit_id = ownership_unit_counters.size() + new_count;
      check(batch.Put(
          container_.family(Family::ownershipUnits),
          slice(set.unit),
          toSlice(unit_id)));
      check(batch.Put(
          container_.family(Family::ownershipUnitIds),
          slice(EncodedNat(unit_id).byteRange()),
          slice(set.unit)));
      ++new_count;
    }

    binary::Output key;
    key.nat(unit_id);
    key.nat(
        unit_id < ownership_unit_counters.size()
            ? ownership_unit_counters[unit_id]
            : 0);
    check(batch.Put(
        container_.family(Family::ownershipRaw),
        slice(key),
        rocksdb::Slice(
            reinterpret_cast<const char*>(set.ids.data()),
            set.ids.size() * sizeof(int64_t))));
  }

  check(container_.db->Write(container_.writeOptions, &batch));

  for (auto i : touched) {
    assert(i < ownership_unit_counters.size());
    ++ownership_unit_counters[i];
  }
  ownership_unit_counters.insert(ownership_unit_counters.end(), new_count, 1);
}

std::unique_ptr<rts::DerivedFactOwnershipIterator>
DatabaseImpl::getDerivedFactOwnershipIterator(Pid pid) {
  struct DerivedFactIterator : rts::DerivedFactOwnershipIterator {
    explicit DerivedFactIterator(Pid pid, std::unique_ptr<rocksdb::Iterator> i)
        : pid_(pid), iter(std::move(i)) {}

    folly::Optional<DerivedFactOwnership> get() override {
      if (iter->Valid()) {
        binary::Input key(byteRange(iter->key()));
        auto pid = key.trustedNat();
        if (pid != pid_.toWord()) {
          return {};
        }
        const auto val = iter->value();
        const size_t elts = val.size() / (sizeof(uint32_t) + sizeof(uint64_t));
        const Id* ids = reinterpret_cast<const Id*>(val.data());
        const UsetId* owners = reinterpret_cast<const UsetId*>(
            val.data() + elts * sizeof(uint64_t));
        iter->Next();
        return rts::DerivedFactOwnership{{ids, elts}, {owners, elts}};
      } else {
        return folly::none;
      }
    }

    Pid pid_;
    std::unique_ptr<rocksdb::Iterator> iter;
  };

  std::unique_ptr<rocksdb::Iterator> iter(container_.db->NewIterator(
      rocksdb::ReadOptions(), container_.family(Family::ownershipDerivedRaw)));

  if (!iter) {
    rts::error("rocksdb: couldn't allocate derived ownership iterator");
  }

  EncodedNat key(pid.toWord());
  iter->Seek(slice(key.byteRange()));
  return std::make_unique<DerivedFactIterator>(pid, std::move(iter));
}

std::unique_ptr<rts::OwnershipUnitIterator>
DatabaseImpl::getOwnershipUnitIterator() {
  struct UnitIterator : rts::OwnershipUnitIterator {
    explicit UnitIterator(std::unique_ptr<rocksdb::Iterator> i)
        : iter(std::move(i)) {}

    folly::Optional<rts::OwnershipUnit> get() override {
      if (iter->Valid()) {
        binary::Input key(byteRange(iter->key()));
        auto unit = key.trustedNat();
        const auto val = iter->value();
        iter->Next();
        return rts::OwnershipUnit{
            static_cast<uint32_t>(unit),
            {reinterpret_cast<const OwnershipUnit::Ids*>(val.data()),
             val.size() / sizeof(OwnershipUnit::Ids)}};
      } else {
        return {};
      }
    }

    std::unique_ptr<rocksdb::Iterator> iter;
  };
  std::unique_ptr<rocksdb::Iterator> iter(container_.db->NewIterator(
      rocksdb::ReadOptions(), container_.family(Family::ownershipRaw)));

  if (!iter) {
    rts::error("rocksdb: couldn't allocate ownership unit iterator");
  }

  iter->SeekToFirst();
  return std::make_unique<UnitIterator>(std::move(iter));
}

void DatabaseImpl::storeOwnership(ComputedOwnership& ownership) {
  container_.requireOpen();

  if (ownership.sets_.size() > 0) {
    auto t = makeAutoTimer("storeOwnership(sets)");
    rocksdb::WriteBatch batch;

    uint32_t id = ownership.firstId_;
    for (auto& exp : ownership.sets_) {
      if ((id % 1000000) == 0) {
        VLOG(1) << "storeOwnership: " << id;
      }
      putOwnerSet(container_, batch, id, exp.op, exp.set);
      id++;
    }
    VLOG(1) << "storeOwnership: writing sets (" << ownership.sets_.size()
            << ")";
    check(container_.db->Write(container_.writeOptions, &batch));
  }

  // ToDo: just update usets_, don't load the whole thing
  usets_ = loadOwnershipSets();

  if (ownership.facts_.size() > 0) {
    auto t = makeAutoTimer("storeOwnership(facts)");
    rocksdb::WriteBatch batch;
    for (uint64_t i = 0; i < ownership.facts_.size(); i++) {
      auto id = ownership.facts_[i].first;
      auto usetid = ownership.facts_[i].second;
      EncodedNat key(id.toWord());
      EncodedNat val(usetid);
      check(batch.Put(
          container_.family(Family::factOwners),
          slice(key.byteRange()),
          slice(val.byteRange())));
    }
    VLOG(1) << "storeOwnership: writing facts: " << ownership.facts_.size()
            << " intervals";
    check(container_.db->Write(container_.writeOptions, &batch));
  }
}

namespace {

struct StoredOwnership : Ownership {
  explicit StoredOwnership(DatabaseImpl* db) : db_(db) {}

  UsetId getOwner(Id id) override;

  UsetId nextSetId() override {
    return db_->usets_->getNextId();
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
    rocksdb::PinnableSlice val;
    auto s = db_->container_.db->Get(
        rocksdb::ReadOptions(),
        db_->container_.family(Family::ownershipSets),
        slice(EncodedNat(id).byteRange()),
        &val);
    binary::Input inp(byteRange(val));
    if (!s.IsNotFound()) {
      check(s);
      SetExpr<SetU32> exp;
      exp.op = static_cast<SetOp>(inp.trustedNat());
      OwnerSet efset;
      deserializeEliasFano(inp, efset);
      exp.set = SetU32::fromEliasFano(efset);
      return exp;
    } else {
      return folly::none;
    }
  }

  std::unique_ptr<rts::OwnershipSetIterator> getSetIterator() override {
    return impl::getSetIterator(db_->container_);
  }

  OwnershipStats getStats() override {
    OwnershipStats stats;
    stats.num_units = db_->ownership_unit_counters.size();
    stats.units_size = 0; // TODO
    if (db_->usets_) {
      stats.num_sets = db_->usets_->size();
      stats.sets_size = db_->usets_->statistics().bytes;
    }
    stats.num_owner_entries = 0; // TODO
    stats.owners_size = 0; // TODO

    return stats;
  }

 private:
  DatabaseImpl* db_;
};

}

const DatabaseImpl::FactOwnerCache::Page* FOLLY_NULLABLE
DatabaseImpl::FactOwnerCache::getPage(
    ContainerImpl& container,
    uint64_t prefix) {
  auto cachePtr = cache_.ulock();
  auto cache = cachePtr->get();

  if (!cache) {
    return nullptr;
  }

  if (prefix >= cache->size() || !(*cache)[prefix]) {
    auto page = std::make_unique<FactOwnerCache::Page>();
    size_t size = 0;

    std::unique_ptr<rocksdb::Iterator> iter(container.db->NewIterator(
        rocksdb::ReadOptions(), container.family(Family::factOwners)));
    EncodedNat prefixKey(prefix << 16);

    iter->Seek(slice(prefixKey.byteRange()));

    while (iter->Valid()) {
      binary::Input key(byteRange(iter->key()));
      rts::Id factId = Id::fromWord(key.trustedNat());
      binary::Input val(byteRange(iter->value()));
      UsetId usetId = val.trustedNat();
      if ((factId.toWord() >> 16) != prefix) {
        break;
      }
      page->factIds.push_back(factId.toWord() & 0xffff);
      page->setIds.push_back(usetId);
      iter->Next();
      size += sizeof(uint16_t) + sizeof(UsetId);
    }

    size_ += size;

    auto wlock = cachePtr.moveFromUpgradeToWrite();
    auto wcache = wlock->get();
    if (prefix >= wcache->size()) {
      wcache->resize(prefix + 1);
    }
    (*wcache)[prefix] = std::move(page);

    VLOG(1) << "FactOwnerCache::getPage(" << prefix << ") size = "
            << folly::prettyPrint(size_, folly::PRETTY_BYTES_IEC);
    return (*wcache)[prefix].get();
  }

  return (*cache)[prefix].get();
}

std::unique_ptr<rts::Ownership> DatabaseImpl::getOwnership() {
  container_.requireOpen();
  return std::make_unique<StoredOwnership>(this);
}

UsetId StoredOwnership::getOwner(Id id) {
  return db_->factOwnerCache_.getOwner(db_->container_, id);
}

void DatabaseImpl::FactOwnerCache::enable() {
  auto cache = cache_.ulock();
  if (*cache) {
    return;
  }

  auto wcache = cache.moveFromUpgradeToWrite();
  *wcache = std::make_unique<FactOwnerCache::Cache>();
  size_ = 0;
}

UsetId DatabaseImpl::FactOwnerCache::getOwner(ContainerImpl& container, Id id) {
  // first find the right page
  auto prefix = id.toWord() >> 16;
  const auto* page = getPage(container, prefix);

  if (!page) {
    // cache is not enabled; fall back to reading from the DB.
    std::unique_ptr<rocksdb::Iterator> iter(container.db->NewIterator(
        rocksdb::ReadOptions(), container.family(Family::factOwners)));

    EncodedNat key(id.toWord());
    iter->SeekForPrev(slice(key.byteRange()));
    if (iter->Valid()) {
      binary::Input val(byteRange(iter->value()));
      return val.trustedNat();
    } else {
      return INVALID_USET;
    }
  }

  // next binary-search on the content of the page to find the
  // interval containing the desired fact ID.
  uint32_t low, high, mid;
  uint16_t ix = id.toWord() & 0xffff;

  low = 0;
  high = page->factIds.size();

  // if this page has no mapping for this fact Id, we have to look in
  // a previous page.
  auto prevPage = [&, prefix]() {
    auto prevPrefix = prefix;
    while (prevPrefix > 0) {
      prevPrefix--;
      const auto& prevPage = getPage(container, prevPrefix);
      if (prevPage->setIds.size() > 0) {
        return prevPage->setIds[prevPage->setIds.size() - 1];
      }
    }
    return INVALID_USET;
  };

  if (high == 0) {
    return prevPage();
  }

  // low is inclusive, high is exclusive
  // low..high always contains an element that is <= id, if there is one
  while (high - low > 1) {
    mid = (high + low) / 2;
    auto x = page->factIds[mid];
    if (x == ix) {
      return page->setIds[mid];
    }
    if (x < ix) {
      low = mid;
    } else {
      high = mid;
    }
  }

  if (page->factIds[low] <= ix) {
    return page->setIds[low];
  } else {
    return prevPage();
  }
}

void DatabaseImpl::cacheOwnership() {
  factOwnerCache_.enable();
}

void DatabaseImpl::addDefineOwnership(DefineOwnership& def) {
  auto t = makeAutoTimer("addDefineOwnership");
  container_.requireOpen();

  VLOG(1) << "addDefineOwnership: "
          << def.owners_.size() + def.new_owners_.size() << " owners, "
          << def.usets_.size() << " sets";

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

    rocksdb::WriteBatch batch;
    size_t numNewSets = 0;

    for (auto uset : def.newSets_) {
      std::set<UsetId> s;
      uset->exp.set.foreach([&](uint32_t elt) { s.insert(subst(elt)); });
      SetU32 set = SetU32::from(s);

      auto newUset = std::make_unique<Uset>(std::move(set), uset->exp.op, 0);
      auto p = newUset.get();
      auto oldId = uset->id;
      auto q = usets_->add(std::move(newUset));
      if (p == q) {
        usets_->promote(p);
        auto ownerset = p->toEliasFano();
        putOwnerSet(container_, batch, p->id, ownerset.op, ownerset.set);
        ownerset.set.free();
        numNewSets++;
      }
      VLOG(2) << "rebased set " << oldId << " -> " << q->id;
      substitution[oldId] = q->id;
    }
    VLOG(1) << "addDefineOwnership: writing sets (" << numNewSets << ")";
    check(container_.db->Write(container_.writeOptions, &batch));

    for (auto& owner : def.owners_) {
      owner = subst(owner);
    }
  }

  // ownershipDerivedRaw :: (Pid,nat) -> vector<int64_t>
  //
  // Similarly to ownershipRaw, this is basically just an
  // append-only log. The nat in the key is a per-Pid counter that
  // we bump by one each time we add another batch of data for a
  // Pid.

  binary::Output key;
  key.nat(def.pid_.toWord());
  const auto [it, _] =
      ownership_derived_counters.insert({def.pid_.toWord(), 0});
  key.nat(it->second++);

  rocksdb::WriteBatch batch;

  binary::Output val;

  val.bytes(
      def.ids_.data(),
      def.ids_.size() *
          sizeof(std::remove_reference<decltype(def.ids_)>::type::value_type));
  val.bytes(
      def.new_ids_.data(),
      def.new_ids_.size() *
          sizeof(
              std::remove_reference<decltype(def.new_ids_)>::type::value_type));
  val.bytes(
      def.owners_.data(),
      def.owners_.size() *
          sizeof(
              std::remove_reference<decltype(def.owners_)>::type::value_type));
  val.bytes(
      def.new_owners_.data(),
      def.new_owners_.size() *
          sizeof(std::remove_reference<
                 decltype(def.new_owners_)>::type::value_type));

  check(batch.Put(
      container_.family(Family::ownershipDerivedRaw), slice(key), slice(val)));

  check(container_.db->Write(container_.writeOptions, &batch));

  VLOG(1) << "addDefineOwnership wrote "
          << def.ids_.size() + def.new_ids_.size() << " entries for pid "
          << def.pid_.toWord();
}



}
}
}
} // namespace facebook
