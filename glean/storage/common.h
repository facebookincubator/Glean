/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include <fmt/core.h>
#include <folly/lang/Assume.h>
#include <array>
#include <cstring>
#include <optional>
#include <utility>

#include <glean/rts/ownership/uset.h>
#include "glean/rts/binary.h"
#include "glean/rts/timer.h"

#include "glean/storage/db.h"

namespace facebook {
namespace glean {
namespace db {

using namespace rts;

/** Keys for the admin column family, which stores per-DB scalar metadata. */
enum class AdminId : uint32_t {
  /** Next fact ID to be allocated (first free ID). */
  NEXT_ID,
  /** Schema representation version of this DB. */
  VERSION,
  /** Lowest fact ID in this DB (first valid ID). */
  STARTING_ID,
  /** First ID allocated by this DB in the shared unit/set namespace. */
  FIRST_UNIT_ID,
  /** Next available ID in the shared unit/set namespace (covers both
   *  UnitIds and UsetIds). Despite the name, this is NOT just the next
   *  unit — it is the end of the entire allocated ID range. */
  NEXT_UNIT_ID,
  /** Number of facts with no ownership unit after storeOwnership(). */
  ORPHAN_FACTS,
  /** Next unused accessibility unit/set ID; unset if none is stored. */
  NEXT_ACCESSIBILITY_ID,
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

/// The names of the admin keys, indexed by AdminId, for error messages.
inline constexpr const char* admin_names[] = {
    "NEXT_ID",
    "VERSION",
    "STARTING_ID",
    "FIRST_UNIT_ID",
    "NEXT_UNIT_ID",
    "ORPHAN_FACTS",
    "NEXT_ACCESSIBILITY_ID",
};

/// Reads the value of an admin key, or none if it isn't set. Fails if the
/// value is shorter than a T, and reports a corrupt database if bytes remain.
template <typename T, typename C>
folly::Optional<T> readAdminValue(C& container, AdminId id) {
  container.requireOpen();
  binary::Output key;
  key.fixed(id);
  folly::Optional<T> result;
  container.get(C::Family::admin, key.bytes(), [&](folly::ByteRange bytes) {
    binary::Input value(bytes);
    result = value.fixed<T>();
    if (!value.empty()) {
      rts::error(
          "corrupt database - invalid {}",
          admin_names[static_cast<uint32_t>(id)]);
    }
  });
  return result;
}

namespace {

/// Write one expression into the selected ownership table.
template <typename C>
void putOwnerSet(
    typename C::Writer& writer,
    const typename C::Family& family,
    UsetId id,
    SetOp op,
    const OwnerSet& set) {
  binary::Output key;
  key.nat(id);
  binary::Output value;
  value.nat(op);
  serializeEliasFano(value, set);
  writer.put(family, key.bytes(), value.bytes());
}

/// Write promoted expressions under their assigned IDs. `upper` is the
/// exclusive end of their ID range and the Elias-Fano universe size.
template <typename C>
void putOwnerSets(
    typename C::Writer& writer,
    const typename C::Family& family,
    Usets& sets,
    UsetId upper) {
  auto id = sets.getFirstId();
  for (auto& expression : sets.toEliasFano(upper)) {
    if ((id % 1000000) == 0) {
      VLOG(1) << "storeOwnership: " << id;
    }
    putOwnerSet<C>(writer, family, id++, expression.op, expression.set);
    expression.set.free();
  }
}

} // namespace

/**
 * The owner sets of one ownership relation stored in a DB, for adding new
 * ones. New sets take their IDs from the relation's ID counter, which they
 * share with its units. Must only be used under the write lock.
 */
template <typename C>
class OwnerSets {
 public:
  using Writer = typename C::Writer;

  /** `family` stores the sets, and the admin key `nextIdKey` persists `next`,
   *  the relation's ID counter. `stored` are the sets already in the DB. */
  OwnerSets(
      const typename C::Family& family,
      AdminId nextIdKey,
      UsetId& next,
      StoredUsets stored)
      : family_(family),
        nextIdKey_(nextIdKey),
        next_(next),
        stored_(std::move(stored)) {}

  /** The ID of the stored set equal to `uset`, or INVALID_USET. */
  UsetId find(Uset* uset) const {
    const auto existing = stored_.lookup(uset);
    return existing ? existing->id : INVALID_USET;
  }

  /** The ID of the stored set equal to `set`. If there is none, writes `set`
   *  under the next ID. */
  UsetId intern(SetExpr<SetU32> set, Writer& writer) {
    auto [stored, added] = stored_.add(
        std::make_unique<Uset>(std::move(set.set), set.op, 0), next_);
    if (added) {
      ++next_;
      auto serialized = stored->toEliasFano(next_);
      putOwnerSet<C>(
          writer, family_, stored->id, serialized.op, serialized.set);
      serialized.set.free();
      putNextId(writer);
    }
    return stored->id;
  }

  /** Writes `sets`, which were numbered from the counter's value or above
   *  when they were computed. */
  void commit(Usets&& sets, Writer& writer) {
    const auto id = sets.getFirstId();
    if (id < next_) {
      rts::error("can't store sets from {}: IDs up to {} are taken", id, next_);
    }
    const auto upper = static_cast<UsetId>(id + sets.size());
    putOwnerSets<C>(writer, family_, sets, upper);
    VLOG(1) << "storeOwnership: writing sets (" << sets.size() << ")";
    next_ = upper;
    putNextId(writer);
    stored_.add(std::move(sets));
  }

 private:
  void putNextId(Writer& writer) const {
    writer.put(C::Family::admin, bytesOf(nextIdKey_), bytesOf(next_));
  }

  const typename C::Family& family_;
  AdminId nextIdKey_;
  UsetId& next_;
  StoredUsets stored_;
};

/// Where an ownership kind is stored, and its state for writing.
template <typename C>
struct OwnershipKindStorage {
  /// Unit name -> UnitId, stored as a raw uint32_t.
  const typename C::Family& units;
  /// UnitId -> unit name, for a kind that can add units after sets. Its
  /// unit count then comes from the highest named unit; otherwise sets follow
  /// the units, and the count ends at the first set.
  const typename C::Family* unitNames;
  /// UsetId -> (Operation, [UsetId])
  const typename C::Family& sets;
  /// An interval map from fact IDs to owners.
  const typename C::Family& owners;
  /// Pages of fact owners for reads after finalization, if the kind
  /// keeps them.
  const typename C::Family* ownerPages;
  /// The admin key of the number of facts without an owner, if the kind
  /// counts them.
  std::optional<AdminId> orphanFactsKey;
  /// The kind's first unit ID.
  const UnitId& firstUnit;
  /// The admin key that persists `nextId`.
  AdminId nextIdKey{};
  /// The next unused ID of the kind's units and sets.
  UsetId& nextId;
  /// The stored sets, while the DB is writable.
  std::unique_ptr<OwnerSets<C>>& writableSets;
  /// Append-only log of derived facts' owners:
  /// (Pid, nat) -> ([Fid], [UsetId])
  const typename C::Family& derivedRaw;
  /// Per-predicate counters for the keys of `derivedRaw`.
  folly::F14FastMap<uint64_t, size_t>& derivedCounters;
};

/// The kind's tables in units, unit names, sets, owners and owner-pages
/// order. Optional tables have a nullptr entry.
template <typename C>
auto ownershipStatsTables(const OwnershipKindStorage<C>& storage) {
  return std::array{
      &storage.units,
      storage.unitNames,
      &storage.sets,
      &storage.owners,
      storage.ownerPages};
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

  /** First ID allocated by this DB in the shared unit/set namespace.
   *  Loaded from AdminId::FIRST_UNIT_ID. */
  rts::UnitId first_unit_id;

  /** Next available ID in the shared unit/set namespace. UnitIds and
   *  UsetIds are allocated from a single contiguous uint32_t range:
   *  [first_unit_id .. next_uset_id). Despite the name this covers
   *  both units AND promoted sets. Persisted as AdminId::NEXT_UNIT_ID. */
  rts::UsetId next_uset_id;

  /// Next unused accessibility unit/set ID. Zero means no stored accessibility.
  /// A stored table contains a public unit, so this is positive even when
  /// that unit's ID is zero.
  rts::UsetId next_accessibility_id{};

  /// The first accessibility unit, `public`, when accessibility is stored.
  rts::UnitId first_accessibility_unit{};

  /** Per-unit batch count: ownership_unit_counters[i] is the number
   *  of ownership batches that reference unit (first_unit_id + i).
   *  Invariant: next_uset_id == first_unit_id + ownership_unit_counters.size()
   *  (before any sets are stored). */
  std::vector<size_t> ownership_unit_counters;
  folly::F14FastMap<uint64_t, size_t> ownership_derived_counters;
  folly::F14FastMap<uint64_t, size_t> accessibility_derived_counters;

  /** The stored visibility owner sets, used during writes to deduplicate and
   *  add sets. Must only be accessed under the write lock. */
  std::unique_ptr<OwnerSets<C>> usets_;

  /// The stored accessibility owner sets, the counterpart of usets_. Null for
  /// read-only DBs and until accessibility ownership is stored.
  std::unique_ptr<OwnerSets<C>> accessibility_usets_;

 protected:
  /// Where visibility ownership is stored.
  OwnershipKindStorage<C> visibilityStorage();

  /// Where accessibility ownership is stored.
  OwnershipKindStorage<C> accessibilityStorage();

  /// Storage tables and state for the requested ownership kind.
  OwnershipKindStorage<C> ownershipStorage(OwnershipKind kind);

 public:
  std::vector<size_t> loadOwnershipUnitCounters();
  folly::F14FastMap<uint64_t, size_t> loadDerivedCounters(OwnershipKind kind);

  /// Iterates over the sets stored in `family`. `emptyNextId` is the first ID
  /// reported when the table is empty.
  std::unique_ptr<rts::OwnershipSetIterator> getSetIterator(
      const typename C::Family& family,
      rts::UsetId emptyNextId);

  /// Load the stored sets of a kind, for writing.
  std::unique_ptr<OwnerSets<C>> loadOwnershipSets(
      const OwnershipKindStorage<C>& storage);

  folly::Optional<uint32_t> getUnitId(folly::ByteRange unit) override;

  /// The ID of the unit named `unit` in a units table.
  folly::Optional<uint32_t> readUnitId(
      const typename C::Family& units,
      folly::ByteRange unit);
  folly::Optional<std::string> getUnit(uint32_t unit_id) override;
  /** Returns the highest UnitId that has a name in the ownershipUnitIds
   *  table. WARNING: this does NOT return the next available ID in the
   *  shared namespace — use next_uset_id for that. */
  std::vector<std::pair<std::string, uint32_t>> getUnitsByPrefix(
      folly::ByteRange prefix) override;
  uint32_t nextUnitId(const typename C::Family& unitNames, UnitId firstUnit);

  void addBatchDescriptor(BatchDescriptor batchDescriptor) override;
  void markBatchDescriptorAsWritten(folly::ByteRange location) override;
  bool isBatchDescriptorStored(folly::ByteRange location) override;
  std::vector<BatchDescriptor> getUnprocessedBatchDescriptors() override;

  /** Register ownership units and their fact-ID ranges. Assigns new
   *  UnitIds from next_uset_id for previously unseen unit names, and
   *  advances next_uset_id accordingly. */
  void addOwnership(const std::vector<OwnershipSet>& ownership) override;

  /** Register name-only ownership units (no fact ranges), allocating a
   *  UnitId for each previously-unseen name. Returns the smallest UnitId
   *  among the given names. */
  uint32_t registerUnits(const std::vector<folly::ByteRange>& names) override;

  /** Allocate a fresh UnitId for a previously-unseen unit `name`, writing
   *  the name<->id mappings, and increment `new_count`. The single place
   *  that mints new UnitIds. */
  uint32_t allocNewUnit(
      typename C::Writer& writer,
      folly::ByteRange name,
      size_t& new_count);

  /** After allocating `new_count` new units in `writer`, advance and
   *  persist the shared id namespace (next_uset_id). */
  void advanceUnitIds(typename C::Writer& writer, size_t new_count);

  /** Write a single ownershipRaw row: key = (unit_id, counter), value =
   *  the fact-id bytes (empty for a unit that owns no facts). */
  void writeOwnershipRaw(
      typename C::Writer& writer,
      uint32_t unit_id,
      size_t counter,
      folly::ByteRange ids);

  std::unique_ptr<rts::DerivedFactOwnershipIterator>
  getDerivedFactOwnershipIterator(OwnershipKind kind, Pid pid) override;

  std::unique_ptr<rts::OwnershipUnitIterator> getOwnershipUnitIterator()
      override;

  /** Persist computed ownership of a relation: serializes promoted
   *  sets to Elias-Fano, writes the fact→UsetId interval map, and
   *  advances the relation's next ID past all stored set IDs. */
  void storeOwnership(OwnershipKind kind, ComputedOwnership& ownership)
      override;

  /// Writes computed ownership into a kind's tables: the promoted sets,
  /// numbered from the kind's next ID, and the fact-owner intervals. An
  /// interval without an owner never replaces a stored owner.
  void storeComputedOwnership(
      const OwnershipKindStorage<C>& storage,
      ComputedOwnership& ownership);

  void storeAccessibilityOwnership(
      ComputedAccessibilityOwnership& ownership) override;
  UsetId getAccessibilityOwner(Id id) override;
  std::unique_ptr<Ownership> getAccessibilityOwnership() override;

  /** Persist derived-fact ownership. Rebases set IDs from the
   *  DefineOwnership's local namespace into the DB's global namespace
   *  via a substitution map, interning new sets one at a time. */
  void addDefineOwnership(DefineOwnership& def) override;

  /// Rebases the sets a derivation batch promoted, numbered from the
  /// kind's next ID when the batch started, into the kind's stored
  /// sets, then appends the batch's per-predicate owners, rebased the same
  /// way, to the kind's derived log.
  void addDerivedOwners(
      const OwnershipKindStorage<C>& storage,
      const Usets& batchSets,
      std::map<Pid, DefineOwnership::PerPredicate>& owners);

  rts::UsetId getOwner(Id id) override;

  UsetId readFactOwner(const typename C::Family& family, Id id);

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

  /// The size of one table, as estimated by the storage engine.
  struct TableStats {
    /// The number of keys.
    uint64_t entries = 0;
    /// The space the table takes, in bytes.
    uint64_t bytes = 0;
  };

  using TableGroup = decltype(ownershipStatsTables(
      std::declval<const OwnershipKindStorage<C>&>()));
  using TableStatsGroup = std::array<TableStats, std::tuple_size_v<TableGroup>>;

  /// Prepares and measures the table group in one backend operation. Returns
  /// estimates in input order, with zeros for missing optional tables.
  virtual TableStatsGroup tableStats(const TableGroup& tables) = 0;

  /// The stats of a kind. Unit and set counts come from its ID counters;
  /// sizes and the number of owner entries are the storage engine's
  /// estimates.
  OwnershipStats getOwnershipStats(const OwnershipKindStorage<C>& storage);

 protected:
  /// The ID of the first accessibility unit, `public`, or zero when the DB
  /// stores no accessibility ownership.
  rts::UnitId readFirstAccessibilityUnit();

 private:
  /// Validates the computed unit and expression ID ranges before storage:
  /// - There must be at least one unit, numbered consecutively from
  ///   firstUnitId_. Expressions must start immediately after the units, at
  ///   firstUnitId_ + units_.size().
  /// - If accessibility is already stored, the computed range's exclusive end
  ///   (first expression ID + number of expressions) must equal the stored
  ///   next_accessibility_id.
  /// Throws if either check fails, before storeAccessibilityOwnership writes
  /// any tables.
  void validateAccessibilityIdRange(
      const ComputedAccessibilityOwnership& ownership) const;

  /// Writes the accessibility units and starts the accessibility ID counter
  /// after them, with an empty cache of stored sets.
  void storeAccessibilityUnits(
      UnitId firstUnit,
      const std::vector<std::string>& units);
};

template <typename C>
OwnershipStats DatabaseCommon<C>::getOwnershipStats(
    const OwnershipKindStorage<C>& storage) {
  container_.requireOpen();
  const auto [units, unitNames, sets, owners, ownerPages] =
      tableStats(ownershipStatsTables(storage));

  OwnershipStats stats{};
  if (storage.unitNames) {
    stats.num_units = nextUnitId(*storage.unitNames, storage.firstUnit) -
        storage.firstUnit; // accurate
    stats.num_sets = storage.nextId - stats.num_units; // accurate
  } else {
    const auto firstSet =
        getSetIterator(storage.sets, storage.nextId)->sizes().first;
    stats.num_units = firstSet - storage.firstUnit; // accurate
    stats.num_sets = storage.nextId - firstSet; // accurate
  }
  stats.units_size = units.bytes + unitNames.bytes; // estimate
  stats.sets_size = sets.bytes; // estimate
  stats.num_owner_entries = owners.entries + ownerPages.entries; // estimate
  stats.owners_size = owners.bytes + ownerPages.bytes; // estimate

  folly::Optional<int64_t> orphans;
  if (storage.orphanFactsKey) {
    orphans = readAdminValue<int64_t>(container_, *storage.orphanFactsKey);
  }
  stats.num_orphan_facts = orphans ? *orphans : -1;

  return stats;
}

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
OwnershipKindStorage<C> DatabaseCommon<C>::visibilityStorage() {
  return {
      .units = C::Family::ownershipUnits,
      .unitNames = &C::Family::ownershipUnitIds,
      .sets = C::Family::ownershipSets,
      .owners = C::Family::factOwners,
      .ownerPages = &C::Family::factOwnerPages,
      .orphanFactsKey = AdminId::ORPHAN_FACTS,
      .firstUnit = first_unit_id,
      .nextIdKey = AdminId::NEXT_UNIT_ID,
      .nextId = next_uset_id,
      .writableSets = usets_,
      .derivedRaw = C::Family::ownershipDerivedRaw,
      .derivedCounters = ownership_derived_counters};
}

template <typename C>
OwnershipKindStorage<C> DatabaseCommon<C>::accessibilityStorage() {
  return {
      .units = C::Family::accessibilityUnits,
      .unitNames = nullptr,
      .sets = C::Family::accessibilitySets,
      .owners = C::Family::accessibilityOwners,
      .ownerPages = nullptr,
      .orphanFactsKey = std::nullopt,
      .firstUnit = first_accessibility_unit,
      .nextIdKey = AdminId::NEXT_ACCESSIBILITY_ID,
      .nextId = next_accessibility_id,
      .writableSets = accessibility_usets_,
      .derivedRaw = C::Family::accessibilityDerivedRaw,
      .derivedCounters = accessibility_derived_counters};
}

template <typename C>
OwnershipKindStorage<C> DatabaseCommon<C>::ownershipStorage(
    OwnershipKind kind) {
  switch (kind) {
    case OwnershipKind::Visibility:
      return visibilityStorage();
    case OwnershipKind::Accessibility:
      return accessibilityStorage();
  }
  folly::assume_unreachable();
}

template <typename C>
rts::UnitId DatabaseCommon<C>::readFirstAccessibilityUnit() {
  if (next_accessibility_id == 0) {
    return 0;
  }
  const auto unit =
      readUnitId(C::Family::accessibilityUnits, binary::stringRange("public"));
  if (!unit) {
    rts::error("stored accessibility ownership has no public unit");
  }
  return *unit;
}

template <typename C>
folly::F14FastMap<uint64_t, size_t> DatabaseCommon<C>::loadDerivedCounters(
    OwnershipKind kind) {
  container_.requireOpen();
  folly::F14FastMap<uint64_t, size_t> result;

  auto iter = container_.read(ownershipStorage(kind).derivedRaw);

  for (iter.seek_first(); iter.valid(); iter.next()) {
    binary::Input key(iter.key());
    auto pid = key.trustedNat();
    const auto [i, _] = result.insert({pid, 0});
    ++i->second;
  }

  VLOG(1) << "derived fact " << ownershipKindName(kind) << " owners for "
          << result.size() << " pids";
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
    const typename C::Family& family,
    rts::UsetId emptyNextId) {
  struct SetIterator : rts::OwnershipSetIterator {
    SetIterator(
        DatabaseCommon<C>& db,
        const typename C::Family& family,
        rts::UsetId emptyNextId)
        : iter(db.container_.read(family)) {
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
        first_ = emptyNextId;
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

  return std::make_unique<SetIterator>(*this, family, emptyNextId);
}

template <typename C>
std::unique_ptr<OwnerSets<C>> DatabaseCommon<C>::loadOwnershipSets(
    const OwnershipKindStorage<C>& storage) {
  auto t = makeAutoTimer("loadOwnershipSets");

  auto iter = getSetIterator(storage.sets, storage.nextId);
  auto pair = iter->sizes();
  auto first = pair.first;
  auto size = pair.second;

  StoredUsets usets;

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
    usets.add(std::make_unique<Uset>(std::move(set), 0), iterPair->first);
  }
  auto stats = usets.statistics();

  LOG(INFO) << "loadOwnershipSets loaded " << stats.adds << " sets from "
            << storage.sets.name << ", " << stats.bytes << " bytes";

  return std::make_unique<OwnerSets<C>>(
      storage.sets, storage.nextIdKey, storage.nextId, std::move(usets));
}

template <typename C>
folly::Optional<uint32_t> DatabaseCommon<C>::getUnitId(folly::ByteRange unit) {
  return readUnitId(C::Family::ownershipUnits, unit);
}

template <typename C>
folly::Optional<uint32_t> DatabaseCommon<C>::readUnitId(
    const typename C::Family& units,
    folly::ByteRange unit) {
  uint32_t r;
  if (container_.get(units, unit, [&](auto val) {
        assert(val.size() == sizeof(uint32_t));
        r = folly::loadUnaligned<uint32_t>(val.data());
      })) {
    return r;
  } else {
    return folly::none;
  }
}

template <typename C>
uint32_t DatabaseCommon<C>::nextUnitId(
    const typename C::Family& unitNames,
    UnitId firstUnit) {
  auto iter = container_.read(unitNames);

  size_t max_unit_id;

  iter.seek_last();
  if (!iter.valid()) {
    max_unit_id = firstUnit;
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

template <typename C>
std::vector<std::pair<std::string, uint32_t>>
DatabaseCommon<C>::getUnitsByPrefix(folly::ByteRange prefix) {
  std::vector<std::pair<std::string, uint32_t>> result;
  auto iter = container_.read(C::Family::ownershipUnits);
  iter.seek_key(prefix);
  while (iter.valid()) {
    auto key = iter.key();
    if (key.size() < prefix.size() ||
        std::memcmp(key.data(), prefix.data(), prefix.size()) != 0) {
      break;
    }
    auto name = binary::mkString(key);
    auto uid = folly::loadUnaligned<uint32_t>(iter.value().data());
    result.emplace_back(std::move(name), uid);
    iter.next();
  }
  return result;
}

// Allocate a fresh UnitId for a previously-unseen unit `name`, writing
// the name<->id mappings, and increment `new_count`. The single place
// that mints new UnitIds; both addOwnership and registerUnits go through
// it.
template <typename C>
uint32_t DatabaseCommon<C>::allocNewUnit(
    typename C::Writer& writer,
    folly::ByteRange name,
    size_t& new_count) {
  uint32_t unit_id = first_unit_id + ownership_unit_counters.size() + new_count;
  writer.put(C::Family::ownershipUnits, name, bytesOf(unit_id));
  EncodedNat key(unit_id);
  writer.put(C::Family::ownershipUnitIds, key.byteRange(), name);
  ++new_count;
  return unit_id;
}

// After allocating `new_count` new units in `writer`, advance and persist
// the shared id namespace (next_uset_id).
template <typename C>
void DatabaseCommon<C>::advanceUnitIds(
    typename C::Writer& writer,
    size_t new_count) {
  if (new_count > 0) {
    next_uset_id += new_count;
    writer.put(
        C::Family::admin,
        bytesOf(AdminId::NEXT_UNIT_ID),
        bytesOf(next_uset_id));
  }
}

// Write a single ownershipRaw row: key = (unit_id, counter), value = the
// fact-id bytes (empty for a unit that owns no facts).
template <typename C>
void DatabaseCommon<C>::writeOwnershipRaw(
    typename C::Writer& writer,
    uint32_t unit_id,
    size_t counter,
    folly::ByteRange ids) {
  binary::Output key;
  key.nat(unit_id);
  key.nat(counter);
  writer.put(C::Family::ownershipRaw, key.bytes(), ids);
}

// Called once per batch inside Store.commit. Registers ownership units and
// their fact-ID ranges; new UnitIds are minted via allocNewUnit.
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
      unit_id = allocNewUnit(writer, set.unit, new_count);
    }

    size_t counter = unit_id < first_unit_id + ownership_unit_counters.size()
        ? ownership_unit_counters[unit_id - first_unit_id]
        : 0;
    folly::ByteRange val(
        reinterpret_cast<const unsigned char*>(set.ids.data()),
        set.ids.size() * sizeof(int64_t));
    writeOwnershipRaw(writer, unit_id, counter, val);
  }

  advanceUnitIds(writer, new_count);

  writer.commit();

  for (auto i : touched) {
    CHECK_LT(i, first_unit_id + ownership_unit_counters.size());
    ++ownership_unit_counters[i - first_unit_id];
  }
  ownership_unit_counters.insert(ownership_unit_counters.end(), new_count, 1);
  CHECK_EQ(next_uset_id, ownership_unit_counters.size() + first_unit_id);
}

// Register name-only ownership units. Mirrors addOwnership's allocation
// for units with empty fact ranges: writes ownershipUnits /
// ownershipUnitIds / an empty ownershipRaw row, and advances the shared
// id namespace.
//
// Returns the ACL-group boundary (firstACLID): the smallest UnitId at
// which units minted by this call begin. allocNewUnit assigns ids
// starting from exactly first_unit_id + ownership_unit_counters.size(),
// so that value is the boundary. Names that already exist keep their
// previous (smaller) ids and deliberately do NOT lower the boundary --
// otherwise the odbFirstACLID invariant (every id >= firstACLID is an
// ACL group unit, every id below it is a regular ownership unit) would
// be violated. Caller-side precondition: ACL group names should not have
// been minted earlier as regular ownership units.
template <typename C>
uint32_t DatabaseCommon<C>::registerUnits(
    const std::vector<folly::ByteRange>& names) {
  container_.requireOpen();

  // Captured before any allocation: the id of the first unit allocNewUnit
  // would mint, i.e. the ACL-group boundary.
  const uint32_t firstAclId = first_unit_id + ownership_unit_counters.size();

  if (names.empty()) {
    return firstAclId;
  }

  auto writer = container_.write();
  size_t new_count = 0;

  for (const auto& name : names) {
    if (!getUnitId(name).hasValue()) {
      const uint32_t unit_id = allocNewUnit(writer, name, new_count);
      writeOwnershipRaw(writer, unit_id, 0, folly::ByteRange());
    }
  }

  advanceUnitIds(writer, new_count);

  writer.commit();

  ownership_unit_counters.insert(ownership_unit_counters.end(), new_count, 1);
  CHECK_EQ(next_uset_id, ownership_unit_counters.size() + first_unit_id);

  return firstAclId;
}

template <typename C>
std::unique_ptr<rts::DerivedFactOwnershipIterator> DatabaseCommon<
    C>::getDerivedFactOwnershipIterator(OwnershipKind kind, Pid pid) {
  struct DerivedFactIterator : rts::DerivedFactOwnershipIterator {
    DerivedFactIterator(
        Pid pid,
        C& container_,
        const typename C::Family& derivedRaw)
        : pid_(pid), iter(container_.read(derivedRaw)) {
      EncodedNat key(pid.toWord());
      iter.seek_key(key.byteRange());
    }

    folly::Optional<DerivedFactOwnership> get() override {
      if (!initial) {
        iter.next();
      } else {
        initial = false;
      }
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
        return rts::DerivedFactOwnership{{ids, elts}, {owners, elts}};
      } else {
        return folly::none;
      }
    }

    Pid pid_;
    bool initial = true;
    typename C::Iterator iter;
  };

  return std::make_unique<DerivedFactIterator>(
      pid, container_, ownershipStorage(kind).derivedRaw);
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
      if (!initial) {
        iter.next();
      } else {
        initial = false;
      }
      if (iter.valid()) {
        binary::Input key(iter.key());
        auto unit = key.trustedNat();
        const auto val = iter.value();
        return rts::OwnershipUnit{
            static_cast<uint32_t>(unit),
            {reinterpret_cast<const OwnershipUnit::Ids*>(val.data()),
             val.size() / sizeof(OwnershipUnit::Ids)}};
      } else {
        return {};
      }
    }

    bool initial = true;
    typename C::Iterator iter;
  };

  return std::make_unique<UnitIterator>(container_);
}

template <typename C>
void DatabaseCommon<C>::storeOwnership(
    OwnershipKind kind,
    ComputedOwnership& ownership) {
  auto storage = ownershipStorage(kind);
  if (!storage.writableSets) {
    rts::error(
        "can't store {} ownership: no stored sets to extend",
        ownershipKindName(kind));
  }
  storeComputedOwnership(storage, ownership);
}

template <typename C>
void DatabaseCommon<C>::storeComputedOwnership(
    const OwnershipKindStorage<C>& storage,
    ComputedOwnership& ownership) {
  container_.requireOpen();

  if (ownership.sets_.size() > 0) {
    auto t = makeAutoTimer("storeOwnership(sets)");
    typename C::Writer writer = container_.write();
    storage.writableSets->commit(std::move(ownership.sets_), writer);
    writer.commit();
  }

  if (ownership.facts_.size() > 0) {
    auto t = makeAutoTimer("storeOwnership(facts)");

    auto hasOwner = [&](EncodedNat& key) -> bool {
      bool r = false;
      container_.get(storage.owners, key.byteRange(), [&](auto) { r = true; });
      return r;
    };

    typename C::Writer writer = container_.write();
    for (uint64_t i = 0; i < ownership.facts_.size(); i++) {
      auto id = ownership.facts_[i].first;
      auto usetid = ownership.facts_[i].second;
      EncodedNat key(id.toWord());
      if (usetid != INVALID_USET || !hasOwner(key)) {
        if (usetid != INVALID_USET) {
          CHECK_LT(usetid, storage.nextId);
        }
        // This is an interval map, and we might be writing multiple sparse sets
        // of intervals where the gaps are indicated by INVALID_USET. Therefore
        // don't overwrite an existing owner with INVALID_USET.
        EncodedNat val(usetid);
        writer.put(storage.owners, key.byteRange(), val.byteRange());
      }
    }
    VLOG(1) << "storeOwnership: writing facts: " << ownership.facts_.size()
            << " intervals";
    writer.commit();
  }
}

template <typename C>
void DatabaseCommon<C>::validateAccessibilityIdRange(
    const ComputedAccessibilityOwnership& ownership) const {
  const auto firstSet = ownership.owners_.sets_.getFirstId();
  if (ownership.units_.empty() || firstSet < ownership.firstUnitId_ ||
      firstSet - ownership.firstUnitId_ != ownership.units_.size()) {
    rts::error(
        "invalid accessibility unit range: first unit {}, first set {}, {} units",
        ownership.firstUnitId_,
        firstSet,
        ownership.units_.size());
  }
  const auto upper =
      firstSet + static_cast<UsetId>(ownership.owners_.sets_.size());
  if (next_accessibility_id != 0 && next_accessibility_id != upper) {
    rts::error(
        "incompatible accessibility ID range: stored {}, computed {}",
        next_accessibility_id,
        upper);
  }
}

template <typename C>
void DatabaseCommon<C>::storeAccessibilityUnits(
    UnitId firstUnit,
    const std::vector<std::string>& units) {
  auto writer = container_.write();
  auto nextUnit = firstUnit;
  for (const auto& name : units) {
    const auto unit = nextUnit++;
    writer.put(
        C::Family::accessibilityUnits, binary::byteRange(name), bytesOf(unit));
  }
  writer.put(
      C::Family::admin,
      bytesOf(AdminId::NEXT_ACCESSIBILITY_ID),
      bytesOf(nextUnit));
  writer.commit();
  first_accessibility_unit = firstUnit;
  next_accessibility_id = nextUnit;
  accessibility_usets_ = std::make_unique<OwnerSets<C>>(
      C::Family::accessibilitySets,
      AdminId::NEXT_ACCESSIBILITY_ID,
      next_accessibility_id,
      StoredUsets());
}

template <typename C>
void DatabaseCommon<C>::storeAccessibilityOwnership(
    ComputedAccessibilityOwnership& ownership) {
  container_.requireOpen();
  validateAccessibilityIdRange(ownership);
  storeAccessibilityUnits(ownership.firstUnitId_, ownership.units_);
  storeComputedOwnership(accessibilityStorage(), ownership.owners_);
}

template <typename C>
UsetId DatabaseCommon<C>::getAccessibilityOwner(Id id) {
  if (next_accessibility_id == 0 || !id || id >= this->firstFreeId()) {
    return INVALID_USET;
  }
  return readFactOwner(C::Family::accessibilityOwners, id);
}

// Called once per batch. Can't be run in parallel.
template <typename C>
void DatabaseCommon<C>::addDefineOwnership(DefineOwnership& def) {
  auto t = makeAutoTimer("addDefineOwnership");
  container_.requireOpen();
  addDerivedOwners(visibilityStorage(), def.sets(), def.defines_);
}

// Will add data into the kind's tables:
// - sets (ownershipSets or accessibilitySets)
// - derivedRaw (ownershipDerivedRaw or accessibilityDerivedRaw)
// - Family::admin
template <typename C>
void DatabaseCommon<C>::addDerivedOwners(
    const OwnershipKindStorage<C>& storage,
    const Usets& batchSets,
    std::map<Pid, DefineOwnership::PerPredicate>& owners) {
  if (!storage.writableSets) {
    rts::error(
        "can't add derived owners to {}: no stored sets to extend",
        storage.sets.name);
  }

  VLOG(1) << "addDefineOwnership: " << batchSets.size() << " sets for "
          << storage.sets.name;

  // add new owner sets
  if (batchSets.getNextId() > batchSets.getFirstId()) {
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
    const auto firstNewId = storage.nextId;

    batchSets.foreachPromoted([&](const Uset* uset) {
      std::set<UsetId> s;
      uset->exp.set.foreach([&](uint32_t elt) {
        UsetId newelt = subst(elt);
        if (newelt >= storage.nextId) {
          rts::error(
              "set id out of range: {} {} {}", newelt, storage.nextId, elt);
        }
        s.insert(newelt);
      });
      const auto newId =
          storage.writableSets->intern({uset->exp.op, SetU32::from(s)}, writer);
      VLOG(2) << "rebased set " << uset->id << " -> " << newId;
      substitution[uset->id] = newId;
    });

    VLOG(1) << "addDefineOwnership: writing sets ("
            << storage.nextId - firstNewId << ")";
    writer.commit();

    for (auto& [_, pred] : owners) {
      for (auto& owner : pred.owners_) {
        owner = subst(owner);
      }

      for (auto& owner : pred.new_owners_) {
        owner = subst(owner);
      }
    }
  }

  typename C::Writer writer = container_.write();

  for (const auto& [pid, pred] : owners) {
    VLOG(1) << "addDefineOwnership: "
            << pred.owners_.size() + pred.new_owners_.size()
            << " owners, for pid " << pid.toWord();

    // derivedRaw :: (Pid,nat) -> vector<int64_t>
    //
    // Similarly to ownershipRaw, this is basically just an
    // append-only log. The nat in the key is a per-Pid counter that
    // we bump by one each time we add another batch of data for a
    // Pid.

    binary::Output key;
    key.nat(pid.toWord());
    const auto [it, _] = storage.derivedCounters.insert({pid.toWord(), 0});
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

    writer.put(storage.derivedRaw, key.bytes(), val.bytes());

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
    return readFactOwner(C::Family::factOwners, id);
  }
}

/// Read an interval owner. Visibility and accessibility maps encode 32-bit
/// UsetId values as natural numbers, including INVALID_USET for unowned gaps.
template <typename C>
UsetId DatabaseCommon<C>::readFactOwner(
    const typename C::Family& family,
    Id id) {
  auto iter = container_.read(family);
  EncodedNat key(id.toWord());
  iter.seek_key_lower(key.byteRange());
  if (iter.valid()) {
    binary::Input value(iter.value());
    const auto owner = value.trustedNat();
    if (owner > INVALID_USET) {
      rts::error("invalid {}: owner ID out of range: {}", family.name, owner);
    }
    return static_cast<UsetId>(owner);
  } else {
    return INVALID_USET;
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
            VLOG(1) << fmt::format("owner cache index: {} entries", num);
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
      VLOG(2) << fmt::format("orphaned fact(s) {}-{}", prev, id - 1);
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

/// Adapter for the persisted ownership of the kind stored as `storage`.
template <typename C>
struct StoredOwnership : Ownership {
  StoredOwnership(DatabaseCommon<C>* db, OwnershipKindStorage<C> storage)
      : db_(db), storage_(std::move(storage)) {}

  UsetId nextSetId() override {
    return storage_.nextId;
  }

  UsetId lookupSet(Uset* uset) override {
    if (!storage_.writableSets) {
      rts::error("rocksdb: lookupSet on read-only DB");
    }
    return storage_.writableSets->find(uset);
  }

  folly::Optional<SetExpr<SetU32>> getUset(UsetId id) override {
    EncodedNat key(id);
    folly::Optional<SetExpr<SetU32>> r;
    if (db_->container_.get(storage_.sets, key.byteRange(), [&](auto val) {
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
    return db_->getSetIterator(storage_.sets, nextSetId());
  }

  folly::Optional<UnitId> getUnitId(folly::ByteRange unit) override {
    return db_->readUnitId(storage_.units, unit);
  }

  OwnershipStats getStats() override {
    return db_->getOwnershipStats(storage_);
  }

 private:
  DatabaseCommon<C>* db_;
  OwnershipKindStorage<C> storage_;
};

} // namespace

template <typename C>
std::unique_ptr<rts::Ownership> DatabaseCommon<C>::getOwnership() {
  container_.requireOpen();
  return std::make_unique<StoredOwnership<C>>(this, visibilityStorage());
}

template <typename C>
std::unique_ptr<Ownership> DatabaseCommon<C>::getAccessibilityOwnership() {
  container_.requireOpen();
  if (next_accessibility_id == 0) {
    return nullptr;
  }
  return std::make_unique<StoredOwnership<C>>(this, accessibilityStorage());
}

} // namespace db
} // namespace glean
} // namespace facebook
