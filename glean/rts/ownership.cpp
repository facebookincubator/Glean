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

#include <folly/MPMCQueue.h>
#include <folly/container/F14Map.h>
#include <folly/container/F14Set.h>
#include <folly/executors/GlobalExecutor.h>
#include <folly/futures/Future.h>

#include <algorithm>
#include <initializer_list>

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

/** Propagate ownership sets transitively through fact references.
 *
 *  Facts can only reference facts with lower IDs. We iterate in reverse
 *  (highest ID first), so by the time we visit a fact, every higher-ID
 *  fact that references it has already propagated its ownership set down.
 *  The final set for a fact is the union of its own initial set (from the
 *  trie) with the sets propagated from all its transitive referrers.
 *
 *  Merges are deferred via UsetsMerge: for each referenced fact we queue
 *  the referrer's set, and merge them all at once (balanced reduction)
 *  when the reverse iterator reaches the fact.  If the pending queue
 *  exceeds 1 GB, an early flush merges everything queued so far.
 *
 *  For stacked DBs, referenced facts in the base DB (id < min_id) are
 *  tracked in `sparse`; their final set is OR'd with the base DB's
 *  existing owner to form (set || base_owner).
 *
 *  On completion, every fact whose set was materialised has been
 *  promoted (assigned a persistent UsetId in `usets`).
 */
FOLLY_NOINLINE void completeOwnership(
    std::vector<Uset*>& facts,
    folly::F14FastMap<uint64_t, Uset*>& sparse,
    Usets& usets,
    const Inventory& inventory,
    Lookup& lookup,
    Lookup* base_lookup) {
  struct Stats {
    Usets& usets;
    const UsetsMerge::Cache::Stats& mergeStats;
    size_t local_facts = 0;
    size_t base_facts = 0;
    size_t owned_facts = 0;

    void bumpLocal() {
      ++local_facts;
    }

    void bumpBase() {
      ++base_facts;
    }

    void bumpOwned() {
      ++owned_facts;
      if (owned_facts % 1000000 == 0) {
        dump();
      }
    }

    void dump() {
      VLOG(1) << folly::sformat(
          "UsetsMergeCache: {} hits, {} misses, hit rate: {:.2f}%",
          mergeStats.hits,
          mergeStats.misses,
          mergeStats.hitRate());
      auto ustats = usets.statistics();
      VLOG(1) << folly::sformat(
          "{} of {} facts ({} visited in base DBs), {} usets, {} promoted, {} bytes, {} adds, {} dups",
          owned_facts,
          local_facts + base_facts,
          base_facts,
          usets.size(),
          ustats.promoted,
          ustats.bytes,
          ustats.adds,
          ustats.dups);
    }
  };
  UsetsMerge usetsMerge{usets};
  Stats stats{usets, usetsMerge.statistics()};

  // `tracker` is a syscall callback that collects fact IDs referenced
  // by a fact's clause into `refs`. Used by predicate->traverse() below.
  std::vector<Id> refs;
  const auto tracker = syscall([&refs](Id id, Pid) { refs.push_back(id); });

  if (facts.size() == 0) {
    return;
  }

  const auto min_id = lookup.startingId();

  // Resolve a fact ID to its current ownership Uset*.  Local facts
  // (id >= min_id) live in the dense `facts` vector; base-DB facts
  // (id < min_id) are tracked sparsely in an F14 map.
  const auto owner = [&](Id id) -> Uset*& {
    if (id < min_id) {
      return sparse[id.toWord()];
    } else {
      return facts[id.toWord() - min_id.toWord()];
    }
  };

  auto processFact = [&, min_id](Fact::Ref fact) {
    // `set == nullptr` means that the fact doesn't have an ownership set - we
    // might want to make that an error eventually?
    if (auto set = owner(fact.id); set || usetsMerge.contains(fact.id)) {
      auto merged = usetsMerge.addUsetAndMerge(fact.id, set);
      owner(fact.id) = merged;
      if (set) {
        usets.drop(set);
      }
      set = merged;
      usets.promote(set);

      // If the fact is in a base DB, then we record its owner as (set || X)
      // where X is the existing owner of the fact. We only propagate `set`,
      // not `X`, to the facts it refers to.
      if (fact.id < min_id) {
        auto base_owner = base_lookup->getOwner(fact.id);
        if (base_owner != INVALID_USET) {
          auto mergedWithBase = usets.merge(SetU32::from({base_owner}), set);
          usets.promote(mergedWithBase);
          owner(fact.id) = mergedWithBase;
        }
      }

      // Walk the fact's clause to find all referenced fact IDs, populating
      // `refs` via `tracker`.
      const auto* predicate = inventory.lookupPredicate(fact.type);
      assert(predicate);
      predicate->traverse(tracker, fact.clause);

      for (const auto ref : refs) {
        usetsMerge.addUset(ref, set);

        // Memory pressure relief: if the deferred-merge queue exceeds
        // 1 GB, flush it now by merging all pending facts eagerly.
        // The results are stored in owner().
        if (usetsMerge.bytes() > compact_threshold) {
          auto factIds = usetsMerge.factIds();
          for (const auto factId : factIds) {
            auto& me = owner(factId);
            auto result = usetsMerge.addUsetAndMerge(factId, me);
            if (me) {
              usets.drop(me);
            }
            me = result;
          }
        }
      }

      // TODO: We can drop the current's fact ownership info here - could
      // shrink the vector. facts.shrink(fact.id);
      stats.bumpOwned();
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
      stats.bumpLocal();
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
      VLOG(1) << folly::sformat("fetching page: {}", page);
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
  stats.dump();

  // --- Phase 2: propagate into base DB(s) ---
  //
  // Local facts may reference base-DB facts.  Phase 1 queued ownership
  // sets for those via `sparse`.  Now we process them in descending ID
  // order (max-heap), recursively following their own references deeper
  // into the base.  Duplicates are skipped via `prev`.
  std::priority_queue<Id> order;
  for (auto [id, uset] : sparse) {
    order.push(Id::fromWord(id));
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
      stats.bumpBase();
      processFact({id, type, clause});
      // Any base-DB facts referenced by this one also need processing
      for (const auto ref : refs) {
        order.push(ref);
      }
      refs.clear();
    });
  }

  stats.dump();
  usetsMerge.clear();
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

  const auto owner = [&](Id id) -> Uset*& {
    return flattened.dense[id.toWord() - min_id.toWord()];
  };

  completeOwnership(
      flattened.dense, flattened.sparse, usets, inventory, lookup, base_lookup);
  t.log("completeOwnership");

  std::vector<std::pair<Id, UsetId>> factOwners;

  // Collect fact ownership for facts in the base DB(s). This data is sparse,
  // but we currently store it as intervals in factOwners because that's the
  // only way to store fact ownership currently. This is a terrible
  // representation since there are typically almost no adjacent facts with the
  // same owner in this dataset.
  //
  // TODO: use a more suitable representation for the sparse ownership
  std::vector<std::pair<uint64_t, Uset*>> order;
  for (auto& pr : flattened.sparse) {
    order.emplace_back(pr);
  }
  std::sort(order.begin(), order.end());
  Id prev = Id::lowest() - 1;
  UsetId current = INVALID_USET;
  for (auto& pr : order) {
    auto id = Id::fromWord(pr.first);
    auto usetid = pr.second->id;
    VLOG(5) << folly::sformat("sparse owner: {} -> {}", id.toWord(), usetid);
    if (id != prev + 1 || current != usetid) {
      if (id != prev + 1) {
        factOwners.emplace_back(prev + 1, INVALID_USET);
      }
      factOwners.emplace_back(id, usetid);
      current = usetid;
    }
    prev = id;
  }
  // fill the gap between the sparse and dense mappings with INVALID_USET
  if (prev + 1 < min_id) {
    factOwners.emplace_back(prev + 1, INVALID_USET);
    current = INVALID_USET;
  }

  Id id = min_id;
  for (; id < min_id + flattened.dense.size(); id++) {
    auto& set = owner(id);
    auto usetid = set ? set->id : INVALID_USET;
    if (usetid != current) {
      factOwners.emplace_back(id, usetid);
      current = usetid;
    }
  }
  // if dense didn't cover the whole Id range, add a final interval of
  // INVALID_USET
  if (id < max_id - 1 && current != INVALID_USET) {
    factOwners.emplace_back(id, INVALID_USET);
  }

  return std::make_unique<ComputedOwnership>(
      std::move(usets), std::move(factOwners));
}

} // namespace rts
} // namespace glean
} // namespace facebook
