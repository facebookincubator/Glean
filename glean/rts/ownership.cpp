/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "glean/rts/ownership.h"
#include "glean/rts/inventory.h"
#include "glean/rts/lookup.h"
#include "glean/rts/ownership/setu32.h"
#include "glean/rts/ownership/triearray.h"
#include "glean/rts/ownership/uset.h"
#include "glean/rts/timer.h"

#if __x86_64__ // AVX required
#include <immintrin.h>
#else
#include "glean/rts/ownership/fallbackavx.h"
#endif

#include <fmt/core.h>
#include <folly/MPMCQueue.h>
#include <folly/container/F14Map.h>
#include <folly/container/F14Set.h>
#include <folly/executors/GlobalExecutor.h>
#include <folly/futures/Future.h>
#include <folly/lang/Assume.h>

#include <algorithm>
#include <initializer_list>
#include <queue>

namespace facebook {
namespace glean {
namespace rts {

// NOLINTNEXTLINE(facebook-avoid-non-const-global-variables)
static size_t compact_threshold = 1ULL << 30;

// NOLINTNEXTLINE(facebook-avoid-non-const-global-variables)
size_t merge_cache_size = 10000;

void setOwnershipCompactThreshold(size_t threshold) {
  compact_threshold = threshold;
}

size_t getOwnershipCompactThreshold() {
  return compact_threshold;
}

void setOwnershipMergeCacheSize(size_t size) {
  merge_cache_size = size;
}

size_t getOwnershipMergeCacheSize() {
  return merge_cache_size;
}

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

/// Deserialize an OwnerSet from a binary::Input. Note that the
// OwnerSet points to the contents of the binary::Input, so that
// must remain alive as long as the OwnerSet is needed.
OwnerSet deserializeEliasFano(binary::Input& in) {
  OwnerSet set;
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
  return set;
}

const char* ownershipKindName(OwnershipKind kind) {
  switch (kind) {
    case OwnershipKind::Visibility:
      return "visibility";
    case OwnershipKind::Accessibility:
      return "accessibility";
  }
  folly::assume_unreachable();
}

UsetId getOwner(Lookup& lookup, OwnershipKind kind, Id id) {
  switch (kind) {
    case OwnershipKind::Visibility:
      return lookup.getOwner(id);
    case OwnershipKind::Accessibility:
      return lookup.getAccessibilityOwner(id);
  }
  folly::assume_unreachable();
}

namespace {

/**
 * Fill ownership data from an iterator
 *
 * Takes the Unit -> FactId mapping provided by the
 * OwnershipUnitIterator and populates the TrieArray with it, making
 * a FactId -> Set(Unit) mapping.
 */
FOLLY_NOINLINE std::unique_ptr<TrieArray<Uset>> fillOwnership(
    OwnershipUnitIterator* iter,
    uint32_t& firstSetId) {
  struct Stats {
    size_t units = 0;
    size_t intervals = 0;

    void bump(size_t curUnits, size_t is) {
      units = curUnits;
      const auto old_intervals = intervals;
      intervals += is;
      if ((old_intervals / 5000000) != (intervals / 5000000)) {
        dump();
      }
    }

    void dump() {
      VLOG(1) << units << " units, " << intervals << " intervals";
    }
  };

  // A replacement for OwnershipUnit that owns the memory
  struct OwnershipUnitCopy {
    UnitId unit;
    std::vector<OwnershipUnit::Ids> ids;
  };

  using Queue =
      folly::MPMCQueue<folly::Optional<OwnershipUnitCopy>, std::atomic, false>;
  Queue queue(100);
  auto executor = folly::getGlobalCPUExecutor();

  folly::Future<folly::Unit> fetcher = folly::via(executor, [&]() {
    while (const auto d = iter->get()) {
      OwnershipUnitCopy c{d->unit, {d->ids.begin(), d->ids.end()}};
      queue.blockingWrite(std::move(c));
    }
    queue.blockingWrite(folly::none);
  });

  auto utrie = std::make_unique<TrieArray<Uset>>();
  uint32_t last_unit = 0;
  uint32_t max_unit = 0;
  Stats stats;
  folly::Optional<OwnershipUnitCopy> d;
  for (queue.blockingRead(d); d; queue.blockingRead(d)) {
    const auto& data = d.value();
    CHECK_GE(data.unit, last_unit);

    utrie->insert(
        data.ids.data(),
        data.ids.data() + data.ids.size(),
        [&](Uset* FOLLY_NULLABLE prev, uint32_t refs) {
          if (prev != nullptr) {
            if (prev->refs == refs) {
              // Do an in-place append if possible (determined via reference
              // count).
              prev->exp.set.append(data.unit);
              return prev;
            } else {
              auto entry = std::make_unique<Uset>(
                  SetU32(prev->exp.set, SetU32::copy_capacity), refs);
              entry->exp.set.append(data.unit);
              prev->refs -= refs;
              return entry.release();
            }
          } else {
            auto entry = std::make_unique<Uset>(SetU32(), refs);
            entry->exp.set.append(data.unit);
            return entry.release();
          }
        });

    max_unit = std::max(data.unit, max_unit);
    stats.bump(max_unit, data.ids.size());
    last_unit = data.unit;
  }

  stats.dump();

  firstSetId = max_unit + 1;
  return utrie;
}

/** Move the sets from the trie to `Usets`. */
FOLLY_NOINLINE Usets
collectUsets(uint32_t firstUsetId, TrieArray<Uset>& utrie) {
  Usets usets(firstUsetId);
  size_t visits = 0;
  utrie.foreach([&](Uset* entry) -> Uset* {
    ++visits;

    // entry->link() caches usets.add(entry) below
    Uset* p = static_cast<Uset*>(entry->link());

    // visited this Uset before?
    if (p == nullptr) {
      p = usets.add(entry);
      entry->link(p);
    }

    // duplicate set? Update the Trie to point to the canonical one.
    if (p != entry) {
      entry->drop();
      return p;
    } else {
      return nullptr;
    }
  });

  usets.foreach([](Uset* entry) { entry->link(nullptr); });

  VLOG(1) << visits << " visits, " << usets.size() << " usets";
  return usets;
}

/** What the reference walk keeps for one ownership relation.
 *
 *  `facts` holds the owners of the facts in this DB, indexed from its starting
 *  ID, and `sparse` the owners of facts in its base DB(s). Their pointers carry
 *  references baked in by the trie rather than taken with use(), so replacing
 *  an owner drops the old one (see OWNERSHIP.md).
 */
struct OwnershipKindWalk {
  OwnershipKindWalk(
      OwnershipKind kind,
      Usets& usets,
      std::vector<Uset*> facts,
      folly::F14FastMap<uint64_t, Uset*> sparse)
      : kind(kind),
        usets(usets),
        facts(std::move(facts)),
        sparse(std::move(sparse)),
        merge(usets) {}

  /** Resolve a fact ID to its current ownership Uset*. Local facts
   *  (id >= min_id) live in the dense `facts` vector; base-DB facts
   *  (id < min_id) are tracked sparsely in an F14 map. */
  Uset*& owner(Id id, Id min_id) {
    if (id < min_id) {
      return sparse[id.toWord()];
    } else {
      const auto index = id.toWord() - min_id.toWord();
      assert(index < facts.size());
      return facts[index];
    }
  }

  /** Whether the fact has an ownership set or queued sets. Unlike owner(),
   *  this never adds an entry to `sparse`. */
  bool reaches(Id id, Id min_id) {
    if (id >= min_id) {
      const auto index = id.toWord() - min_id.toWord();
      assert(index < facts.size());
      if (facts[index]) {
        return true;
      }
    } else if (const auto entry = sparse.find(id.toWord());
               entry != sparse.end() && entry->second) {
      return true;
    }
    return merge.contains(id);
  }

  /** Merge the fact's initial set with the sets queued by its referrers,
   *  record the result as the fact's owner and promote it. Returns the set
   *  to propagate to the facts it references.
   *
   *  If the fact is in a base DB, then we record its owner as (set || X)
   *  where X is the existing owner of the fact. We only propagate `set`,
   *  not `X`, to the facts it refers to. */
  Uset* settle(Id id, Id min_id, Lookup* base_lookup) {
    auto set = owner(id, min_id);
    auto merged = merge.addUsetAndMerge(id, set);
    owner(id, min_id) = merged;
    if (set) {
      usets.drop(set);
    }
    usets.promote(merged);

    if (id < min_id) {
      auto base_owner = getOwner(*base_lookup, kind, id);
      if (base_owner != INVALID_USET) {
        auto mergedWithBase = usets.merge(SetU32::from({base_owner}), merged);
        usets.promote(mergedWithBase);
        owner(id, min_id) = mergedWithBase;
      }
    }
    ++owned_facts;
    return merged;
  }

  /** Queue `set` for deferred merging into each fact in `refs`. Base-DB
   *  facts with nothing queued before join `order`. */
  void propagate(
      Uset* set,
      const std::vector<Id>& refs,
      Id min_id,
      std::priority_queue<Id>& order) {
    for (const auto ref : refs) {
      const bool newlyQueued = merge.addUset(ref, set);
      if (newlyQueued && ref < min_id) {
        order.push(ref);
      }

      // Memory pressure relief: if the deferred-merge queue exceeds
      // 1 GB, flush it now by merging all pending facts eagerly.
      // The results are stored in owner().
      if (merge.bytes() > compact_threshold) {
        auto factIds = merge.factIds();
        for (const auto factId : factIds) {
          auto& me = owner(factId, min_id);
          auto result = merge.addUsetAndMerge(factId, me);
          if (me) {
            usets.drop(me);
          }
          me = result;
        }
      }
    }
  }

  void dump(size_t visited_facts, size_t base_facts) const {
    const auto& mergeStats = merge.statistics();
    VLOG(1) << fmt::format(
        "{}: UsetsMergeCache: {} hits, {} misses, hit rate: {:.2f}%",
        ownershipKindName(kind),
        mergeStats.hits,
        mergeStats.misses,
        mergeStats.hitRate());
    auto ustats = usets.statistics();
    VLOG(1) << fmt::format(
        "{}: {} of {} facts ({} visited in base DBs), {} usets, {} promoted, {} bytes, {} adds, {} dups",
        ownershipKindName(kind),
        owned_facts,
        visited_facts,
        base_facts,
        usets.size(),
        ustats.promoted,
        ustats.bytes,
        ustats.adds,
        ustats.dups);
  }

  const OwnershipKind kind;
  Usets& usets;
  std::vector<Uset*> facts;
  folly::F14FastMap<uint64_t, Uset*> sparse;
  UsetsMerge merge;
  size_t owned_facts = 0;
};

/** Propagate ownership sets transitively through fact references, for
 *  every relation in `walks`.
 *
 *  Facts can only reference facts with lower IDs. We iterate in reverse
 *  (highest ID first), so by the time we visit a fact, every higher-ID
 *  fact that references it has already propagated its ownership set down.
 *  The final set for a fact is the union of its own initial set (from the
 *  trie) with the sets propagated from all its transitive referrers.
 *
 *  Each fact's clause is traversed once, when any relation has a set or a
 *  queued contribution for it. Every such relation then merges, promotes and
 *  propagates its own sets.
 *
 *  Merges are deferred via UsetsMerge: for each referenced fact we queue
 *  the referrer's set, and merge them all at once (balanced reduction)
 *  when the reverse iterator reaches the fact.  If the pending queue
 *  exceeds 1 GB, an early flush merges everything queued so far.
 *
 *  For stacked DBs, base-DB facts (id < min_id) with claimed or queued sets
 *  are processed next, highest ID first, and their final set is OR'd with
 *  the base DB's existing owner to form (set || base_owner).
 *
 *  On completion, every fact whose set was materialised has been
 *  promoted (assigned a persistent UsetId in its relation's `usets`).
 */
FOLLY_NOINLINE void completeOwnership(
    const std::vector<OwnershipKindWalk*>& walks,
    const Inventory& inventory,
    Lookup& lookup,
    Lookup* base_lookup) {
  size_t local_facts = 0;
  size_t base_facts = 0;
  size_t owned_facts = 0;
  const auto dump = [&] {
    for (const auto* walk : walks) {
      walk->dump(local_facts + base_facts, base_facts);
    }
  };

  // `tracker` is a syscall callback that collects fact IDs referenced
  // by a fact's clause into `refs`. Used by predicate->traverse() below.
  std::vector<Id> refs;
  const auto tracker = syscall([&refs](Id id, Pid) { refs.push_back(id); });

  if (std::all_of(
          walks.begin(), walks.end(), [](const OwnershipKindWalk* walk) {
            return walk->facts.empty();
          })) {
    return;
  }

  const auto min_id = lookup.startingId();

  // Base-DB facts for Phase 2. processFact pushes each base fact it starts
  // queuing sets for.
  std::priority_queue<Id> order;

  // The kinds that reach the current fact, and the sets they propagate.
  std::vector<OwnershipKindWalk*> reaching;
  std::vector<Uset*> propagated;

  auto processFact = [&, min_id](Fact::Ref fact) {
    reaching.clear();
    for (auto* walk : walks) {
      if (walk->reaches(fact.id, min_id)) {
        reaching.push_back(walk);
      }
    }
    // A fact that no kind reaches doesn't have an ownership set - we
    // might want to make that an error eventually?
    if (reaching.empty()) {
      return;
    }

    propagated.clear();
    for (auto* walk : reaching) {
      propagated.push_back(walk->settle(fact.id, min_id, base_lookup));
    }

    // Walk the fact's clause to find all referenced fact IDs, populating
    // `refs` via `tracker`.
    const auto* predicate = inventory.lookupPredicate(fact.type);
    assert(predicate);
    predicate->traverse(tracker, fact.clause);

    for (size_t i = 0; i < reaching.size(); ++i) {
      reaching[i]->propagate(propagated[i], refs, min_id, order);
    }

    // TODO: We can drop the current's fact ownership info here - could
    // shrink the vector. facts.shrink(fact.id);
    if (++owned_facts % 1000000 == 0) {
      dump();
    }
  };

  // --- Phase 1: process local-DB facts in reverse order ---
  //
  // A producer thread fetches pages of facts (1M IDs each) from highest
  // to lowest and pushes them into a bounded queue.  The consumer
  // (this thread) pops each page and calls processFact in reverse order
  // within it.  Double-buffering gives ~2x speedup.

  using FactPage = std::vector<Fact::unique_ptr>;
  auto fetchPage = [&](Id min_id, Id max_id) -> FactPage {
    FactPage page;
    for (auto iter = lookup.enumerate(min_id, max_id); auto fact = iter->get();
         iter->next()) {
      page.emplace_back(Fact::create(iter->get()));
    }
    return page;
  };

  auto processPage = [&](FactPage& page) {
    Fact::unique_ptr f;
    auto it = page.end();
    while (it != page.begin()) {
      --it;
      ++local_facts;
      processFact((*it)->ref());
      refs.clear();
    }
  };

  const uint32_t pageSize = 1024 * 1024;

  using Queue = folly::MPMCQueue<folly::Optional<FactPage>, std::atomic, false>;
  Queue queue(10);
  auto executor = folly::getGlobalCPUExecutor();

  // Producer: fetch pages from highest to lowest ID
  folly::Future<folly::Unit> fetcher = folly::via(executor, [&]() {
    auto pageOf = [](Id id) -> uint64_t {
      return (id.toWord() / pageSize) * pageSize;
    };
    Id start = lookup.startingId();
    Id last = lookup.firstFreeId() - 1;
    uint64_t first = pageOf(start);
    for (uint64_t page = pageOf(last);; page -= pageSize) {
      VLOG(1) << fmt::format("fetching page: {}", page);
      queue.blockingWrite(fetchPage(
          (page == first) ? start : Id::fromWord(page),
          Id::fromWord(page + pageSize)));
      if (page == first) {
        break;
      }
    }
    queue.blockingWrite(folly::none);
  });

  // Consumer: process each page in reverse fact-ID order
  folly::Optional<FactPage> page;
  do {
    queue.blockingRead(page);
    if (page) {
      processPage(page.value());
    }
  } while (page);

  fetcher.wait();
  dump();

  // --- Phase 2: propagate into base DB(s) ---
  //
  // Process base facts with ownership in any kind's `sparse` map or
  // deferred in its UsetsMerge in descending ID order, following their
  // references deeper into the base. Duplicates are skipped via `prev`.
  for (const auto* walk : walks) {
    for (const auto& [id, uset] : walk->sparse) {
      order.push(Id::fromWord(id));
    }
  }
  Id prev = Id::invalid();
  for (; !order.empty(); order.pop()) {
    auto id = order.top();
    if (id == prev) {
      continue;
    } else {
      prev = id;
    }
    base_lookup->factById(id, [&](Pid type, Fact::Clause clause) {
      ++base_facts;
      processFact({id, type, clause});
      refs.clear();
    });
  }

  dump();
  for (auto* walk : walks) {
    walk->merge.clear();
  }
}

/** Encode each fact's owner as intervals of fact IDs: the owners of base-DB
 *  facts first, then those of the facts in this DB, from `min_id` up to
 *  `max_id`. INVALID_USET marks facts without an owner. */
std::vector<std::pair<Id, UsetId>>
ownerIntervals(const OwnershipKindWalk& walk, Id min_id, Id max_id) {
  std::vector<std::pair<Id, UsetId>> factOwners;

  // Collect fact ownership for facts in the base DB(s). This data is sparse,
  // but we currently store it as intervals in factOwners because that's the
  // only way to store fact ownership currently. This is a terrible
  // representation since there are typically almost no adjacent facts with the
  // same owner in this dataset.
  //
  // TODO: use a more suitable representation for the sparse ownership
  std::vector<std::pair<uint64_t, Uset*>> order;
  for (auto& pr : walk.sparse) {
    order.emplace_back(pr);
  }
  std::sort(order.begin(), order.end());
  Id prev = Id::lowest() - 1;
  UsetId current = INVALID_USET;
  for (auto& fact_uset : order) {
    const auto fact_id = Id::fromWord(fact_uset.first);
    const auto usetid = fact_uset.second->id;
    VLOG(5) << fmt::format("sparse owner: {} -> {}", fact_id.toWord(), usetid);
    if (fact_id != prev + 1 // expect fact IDs to be consecutive. Otherwise some
                            // facts are missing.
        || usetid != current // factOwners encode fact ranges, so if the usetid
                             // is the same, no need to emplace back to it
    ) {
      if (fact_id != prev + 1) {
        factOwners.emplace_back(prev + 1, INVALID_USET);
      }
      factOwners.emplace_back(fact_id, usetid);
      current = usetid;
    }
    prev = fact_id;
  }
  // fill the gap between the sparse and dense mappings with INVALID_USET
  if (prev + 1 < min_id) {
    factOwners.emplace_back(prev + 1, INVALID_USET);
    current = INVALID_USET;
  }

  Id id = min_id;
  for (auto* set : walk.facts) {
    auto usetid = set ? set->id : INVALID_USET;
    if (usetid != current) {
      factOwners.emplace_back(id, usetid);
      current = usetid;
    }
    id++;
  }
  // if dense didn't cover the whole Id range, add a final interval of
  // INVALID_USET
  if (id < max_id - 1 && current != INVALID_USET) {
    factOwners.emplace_back(id, INVALID_USET);
  }
  return factOwners;
}
} // namespace

/* Note [stacked incremental DBs]

A stacked DB might induce changes to the ownership of facts in DBs
below it in the stack. So we have to propagate ownership from facts in
the stacked DB to facts in the base DB(s).  The basic approach is

  * Each DB stores ownership information about its own facts and
    potentially facts in base DB(s).

  * getOwner() in a stacked DB traverses the stack downwards,
    returning the first owner found.

  * Therefore, if a fact F has owner A in the base DB and a new owner B
    in the stacked DB, we have to assign it owner A || B.

  * Thus, an owner set in a stacked DB (A || B) may refer to owner
    sets in a base DB (A).

  * A derived fact in the stacked DB will be given the correct owner,
    because its ownership is determined by calling getOwner() on each of
    the facts that it was derived from, even if some of those are in
    a base DB.
*/

/** Compute the complete ownership map for a database.
 *
 *  This is the top-level entry point, orchestrating three phases:
 *
 *  1. fillOwnership: read (unit → fact-ID-ranges) from `iter` and build
 *     a TrieArray mapping each fact to its set of owning units.
 *
 *  2. collectUsets: deduplicate the trie's sets into a Usets store.
 *
 *  3. completeOwnership: propagate ownership transitively through fact
 *     references, promote final sets, and handle stacked-DB merging.
 *
 *  The result is a ComputedOwnership containing the Usets store and a
 *  run-length-encoded factOwners interval map (fact-ID → UsetId).
 *
 *  See Note [stacked incremental DBs] above for how base-DB ownership
 *  interacts with stacked DBs.
 */
std::unique_ptr<ComputedOwnership> computeOwnership(
    const Inventory& inventory,
    Lookup& lookup, // the current DB, *not* stacked
    Lookup* base_lookup, // the base DB stack, if this is a stack
    OwnershipUnitIterator* iter) {
  uint32_t firstUsetId;
  auto t = makeAutoTimer("computeOwnership");
  VLOG(1) << "computing ownership";

  const auto min_id = lookup.startingId();
  const auto max_id = lookup.firstFreeId();

  auto utrie = fillOwnership(iter, firstUsetId);
  t.log("fillOwnership");
  auto usets = collectUsets(firstUsetId, *utrie);
  t.log("collectUsets");
  // TODO: Should `completeOwnership` work with the trie rather than a
  // flat vector?

  auto flattened = utrie->flatten(min_id.toWord(), max_id.toWord());

  // The trie is no longer required, so free it
  utrie.reset();

  OwnershipKindWalk visibility(
      OwnershipKind::Visibility,
      usets,
      std::move(flattened.dense),
      std::move(flattened.sparse));
  completeOwnership({&visibility}, inventory, lookup, base_lookup);
  t.log("completeOwnership");

  auto factOwners = ownerIntervals(visibility, min_id, max_id);

  return std::make_unique<ComputedOwnership>(
      std::move(usets), std::move(factOwners));
}

} // namespace rts
} // namespace glean
} // namespace facebook
