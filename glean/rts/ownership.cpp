/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "glean/rts/lookup.h"
#include "glean/rts/inventory.h"
#include "glean/rts/ownership.h"
#include "glean/rts/ownership/setu32.h"
#include "glean/rts/ownership/triearray.h"
#include "glean/rts/ownership/uset.h"
#include "glean/rts/timer.h"
#include "glean/rts/factset.h"

#if __x86_64__ // AVX required
#include <folly/experimental/EliasFanoCoding.h>
#include <immintrin.h>
#else
#include "glean/rts/ownership/fallbackavx.h"
#endif

#include <folly/container/F14Map.h>
#include <folly/container/F14Set.h>
#include <folly/executors/GlobalExecutor.h>
#include <folly/futures/Future.h>
#include <folly/Hash.h>
#include <folly/MPMCQueue.h>

#include <xxhash.h>

#include <algorithm>
#include <initializer_list>
#include <limits>
#include <type_traits>


namespace facebook {
namespace glean {
namespace rts {

namespace {

/**
 * Fill ownership data from an iterator
 *
 * Takes the Unit -> FactId mapping provided by the
 * OwnershipUnitIterator and populates the TrieArray with it, making
 * a FactId -> Set(Unit) mapping.
 */
FOLLY_NOINLINE TrieArray<Uset> fillOwnership(
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
      VLOG(1)
        << units << " units, "
        << intervals << " intervals";
    }
  };

  TrieArray<Uset> utrie;
  uint32_t last_unit = 0;
  uint32_t max_unit = 0;
  Stats stats;
  while(const auto d = iter->get()) {
    const auto data = d.value();
    CHECK_GE(data.unit,last_unit);

    utrie.insert(
      data.ids.begin(),
      data.ids.end(),
      [&](Uset * FOLLY_NULLABLE prev, uint32_t refs) {
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
      }
    );

    max_unit = std::max(data.unit, max_unit);
    stats.bump(max_unit, data.ids.size());
    last_unit = data.unit;
  }

  stats.dump();

  firstSetId = max_unit + 1;
  return utrie;
}

/** Move the sets from the trie to `Usets`. */
FOLLY_NOINLINE Usets collectUsets(uint32_t firstUsetId, TrieArray<Uset>& utrie) {
  Usets usets(firstUsetId);
  size_t visits = 0;
  utrie.foreach([&](Uset *entry) -> Uset* {
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

  usets.foreach([](Uset *entry) {
    entry->link(nullptr);
  });

  VLOG(1)
    << visits << " visits, "
    << usets.size() << " usets";
  return usets;
}

/** Transitively complete `Usets` by assigning an ownership unit to facts which
 * are transitively referenced by a fact already belonging to that unit.
 *
 * The resulting `Usets` will contain exactly those sets (the "promoted" ones)
 * which describe the ownership of at least one fact.
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
      if (owned_facts%1000000 == 0) {
        dump();
      }
    }

    void dump() {
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

  Stats stats{usets};

  std::vector<Id> refs;
  const auto tracker = syscall([&refs](Id id, Pid) {
    refs.push_back(id);
  });

  if (facts.size() == 0) {
    return;
  }

  // Iterate over facts backwards - this ensures that we get all dependencies.
  const auto min_id = lookup.startingId();
  const auto max_id = lookup.firstFreeId();
  const auto owner = [&](Id id) -> Uset*& {
    if (id < min_id) {
      return sparse[id.toWord()];
    } else {
      return facts[id.toWord() - min_id.toWord()];
    }
  };

  auto processFact = [&, min_id, max_id](Fact::Ref fact) {
    // `set == nullptr` means that the fact doesn't have an ownership set - we
    // might want to make that an error eventually?
    if (auto set = owner(fact.id)) {
      // If the fact is in a base DB, then we record its owner as (set || X)
      // where X is the existing owner of the fact. We only propagate `set`,
      // not `X`, to the facts it refers to.
      if (fact.id < min_id) {
        auto base_owner = base_lookup->getOwner(fact.id);
        if (base_owner != INVALID_USET) {
          auto merged = usets.merge(SetU32::from({base_owner}), set);
          usets.promote(merged);
          owner(fact.id) = merged;
        }
      }

      usets.promote(set);

      // Collect all references to facts
      const auto *predicate = inventory.lookupPredicate(fact.type);
      assert(predicate);
      predicate->traverse(tracker, fact.clause);

      // For each fact we reference, add our ownership set to what we've
      // computed for it so far. Use the `link` field to avoid computing the
      // same set union multiple times.
      //
      // TODO: Try adding a fixed-size LRU (or LFU?) cache for set unions?
      std::vector<Uset *> touched;
      for (const auto id : refs) {
        auto& me = owner(id);
        if (me == nullptr) {
          // The fact didn't have ownership info before, assign the set to it.
          me = set;
          usets.use(me, 1);
        } else if (const auto added = static_cast<Uset *>(me->link())) {
          // The fact did have ownership info and we've already computed the
          // union for that particular set.
          usets.use(added);
          usets.drop(me);
          me = added;
        } else {
          // Compute the union.
          const auto p = usets.merge(me, set);
          me->link(p);
          touched.push_back(me);
          me = p;
        }
      }

      // Reset the link fields. Note that we always add 1 refcount for sets we
      // store in `touched` (to avoid having dangling references there) so drop
      // it here.
      for (const auto p : touched) {
        p->link(nullptr);
        usets.drop(p);
      }

      touched.clear();
      // TODO: We can drop the current's fact ownership info here - could shrink
      // the vector.
      // facts.shrink(fact.id);
      stats.bumpOwned();
    }
  };

  // We process facts in reverse order, so that we only process each fact
  // once. We fetch the facts in parallel with processing them,
  // which speeds up the whole process by about 2x. It should be possible
  // to speed it up further by parallelising the processing.

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

  folly::Future<folly::Unit> fetcher = folly::via(executor, [&]() {
    auto pageOf = [](Id id) {
      return Id::fromWord((id.toWord() / pageSize) * pageSize);
    };
    Id last = pageOf(lookup.firstFreeId() - 1);
    Id first = pageOf(lookup.startingId());
    for (Id id = last;; id -= pageSize) {
      VLOG(1) << folly::sformat("fetching page: {}", id.toWord());
      queue.blockingWrite(fetchPage(id, id + pageSize));
      if (id == first) {
        break;
      }
    }
    queue.blockingWrite(folly::none);
  });

  folly::Optional<FactPage> page;
  do {
    queue.blockingRead(page);
    if (page) {
      processPage(page.value());
    }
  } while (page);

  fetcher.wait();
  stats.dump();

  // Propagate ownership sets through facts in the base DB(s), using a priority
  // queue to process facts in descending order. Note there might be multiple
  // entries in the queue for a given fact ID.
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
      processFact({id,type,clause});
      for (const auto ref : refs) {
        order.push(ref);
      }
      refs.clear();
    });
  }

  stats.dump();
}
}

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

std::unique_ptr<ComputedOwnership> computeOwnership(
    const Inventory& inventory,
    Lookup& lookup, // the current DB, *not* stacked
    Lookup* base_lookup, // the base DB stack, if this is a stack
    OwnershipUnitIterator *iter) {
  uint32_t firstUsetId;
  auto t = makeAutoTimer("computeOwnership");
  VLOG(1) << "computing ownership";
  auto utrie = fillOwnership(iter,firstUsetId);
  t.log("fillOwnership");
  auto usets = collectUsets(firstUsetId,utrie);
  t.log("collectUsets");
  // TODO: Should `completeOwnership` work with the trie rather than a
  // flat vector?

  const auto min_id = lookup.startingId();
  const auto max_id = lookup.firstFreeId();
  auto flattened = utrie.flatten(min_id.toWord(), max_id.toWord());
  const auto owner = [&](Id id) -> Uset*& {
    return flattened.dense[id.toWord() - min_id.toWord()];
  };

  completeOwnership(
      flattened.dense, flattened.sparse, usets, inventory, lookup, base_lookup);
  t.log("completeOwnership");

  std::vector<std::pair<Id,UsetId>> factOwners;

  // Collect fact ownership for facts in the base DB(s). This data is sparse,
  // but we currently store it as intervals in factOwners because that's the
  // only way to store fact ownership currently. This is a terrible
  // representation since there are typically almost no adjacent facts with the
  // same owner in this dataset.
  //
  // TODO: use a more suitable representation for the sparse ownership
  std::vector<std::pair<uint64_t, Uset*>> order;
  for (auto &pr : flattened.sparse) {
    order.push_back(pr);
  }
  std::sort(order.begin(),order.end());
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
  // if dense didn't cover the whole Id range, add a final interval of INVALID_USET
  if (id < max_id-1 && current != INVALID_USET) {
    factOwners.emplace_back(id, INVALID_USET);
  }

  auto sets = usets.toEliasFano();

  return std::make_unique<ComputedOwnership>(
      usets.getFirstId(),
      std::move(sets),
      std::move(factOwners));
}

}
}
}
