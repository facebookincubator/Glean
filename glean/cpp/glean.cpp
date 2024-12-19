/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "glean/cpp/glean.h"
#include <folly/gen/Base.h>
#include <algorithm>
#include <iterator>
#include "glean/rts/sanity.h"

namespace facebook {
namespace glean {
namespace cpp {

BatchBase::BatchBase(const SchemaInventory* inv, size_t cache_capacity)
    : inventory(inv),
      stats(std::make_shared<rts::LookupCache::Stats>()),
      cache(
          rts::LookupCache::Options{
              cache_capacity,
              0 // disable deferred LRU list manipulation
          },
          stats),
      anchor(&rts::EmptyLookup::instance(), &cache),
      buffer(Id::lowest()),
      facts(&anchor, &buffer) {}

rts::FactSet::Serialized BatchBase::serialize() const {
  return buffer.serialize();
}

std::map<std::string, std::vector<int64_t>> BatchBase::serializeOwnership()
    const {
  std::map<std::string, std::vector<int64_t>> ow;
  for (const auto& p : owned) {
    std::vector<int64_t> ids;
    ids.reserve(p.facts.iterative_size() * 2);
    for (auto ival : p.facts) {
      ids.push_back(ival.lower().toThrift());
      ids.push_back(ival.upper().toThrift());
    }
    ow.insert({p.unit, std::move(ids)});
  }
  // it's possible that ow.size() != owned.size() here. If we indexed
  // the same file multiple times, then we would have ownership data
  // for the same unit occurring multiple times in the owned deque.
  last_serialized_units = owned.size();
  total_serialized_units += owned.size();
  return ow;
}

void BatchBase::clearOwnership() {
  owned.clear();
}

void BatchBase::rebase(const rts::Substitution& subst) {
  GLEAN_SANITY_CHECK(subst.sanityCheck(false));
  std::unique_ptr<rts::Substitution> usubst;
  cache.withBulkStore([&](auto& store) {
    auto r = buffer.rebase(inventory->inventory, subst, store);
    buffer = std::move(r.first);
    usubst = std::make_unique<rts::Substitution>(std::move(r.second));
    facts = rts::Stacked<rts::Define>(&anchor, &buffer);
  });
  GLEAN_SANITY_CHECK(buffer.sanityCheck());

  for (auto& p : owned) {
    p.facts = usubst->substIntervals(p.facts);
  }
}

BatchBase::CacheStats BatchBase::cacheStats() {
  auto values = stats->read();
  CacheStats res;
  res.facts = FactStats{
      values[rts::LookupCache::Stats::factBytes],
      values[rts::LookupCache::Stats::factCount]};
  res.hits = values[rts::LookupCache::Stats::idByKey_hits] +
      values[rts::LookupCache::Stats::typeById_hits] +
      values[rts::LookupCache::Stats::factById_hits];
  res.misses = values[rts::LookupCache::Stats::idByKey_misses] +
      values[rts::LookupCache::Stats::idByKey_failures] +
      values[rts::LookupCache::Stats::typeById_misses] +
      values[rts::LookupCache::Stats::typeById_failures] +
      values[rts::LookupCache::Stats::factById_misses] +
      values[rts::LookupCache::Stats::factById_failures];
  return res;
}

void BatchBase::beginUnit(std::string unit) {
  owned.push_back({std::move(unit), {}, buffer.firstFreeId(), Id::invalid()});
  current = &owned.back();
}

/*
  Minimizing ownership data

  We start with all the facts produced for a translation unit in a single
  ownership interval

  [A, B)

  The fact batch goes through a few rounds of de-duplication on its
  way to the DB. If we end up de-duplicating some of the facts, the ownership
  data remains, so we end up with

  [a,b) [c,d) [e,f) [A, B)

  large numbers of intervals are expensive to process each time we de-duplicate
  this batch.

  So what we do is to partition the facts into two: "root" facts that are not
  reachable from any other fact, and facts that are reachable. As long as the
  root facts are covered by ownership, we don't need to cover the reachable
  facts since the ownership will be automatically propagated by the server
  later. We sort the facts so that the root facts are all together, meaning
  we still have a single ownership interval for the batch.

  By having fewer facts covered by ownership, we will have fewer
  ownership intervals to process. This speeds up writing, and also results in
  less ownership data to process on the server during the propagation phase.
 */
std::pair<rts::closed_interval_set<Id>, rts::FactSet> minimalOwnership(
    const rts::Inventory& inventory,
    rts::FactSet& facts,
    rts::LookupCache::Anchor& anchor,
    Id start,
    Id end) {
  rts::closed_interval_set<Id> owned;
  owned.add({start, end - 1});

  // iterate through the facts in reverse order
  // remove referenced facts from the interval_set
  {
    const auto eachRef = rts::syscall([&owned, start, end](Id id, Pid) {
      if (id >= start && id < end) {
        owned.erase(id);
      }
    });

    auto it = facts.enumerateBack(end, start);
    while (auto fact = it->get()) {
      const auto* predicate = inventory.lookupPredicate(fact.type);
      assert(predicate);
      predicate->traverse(eachRef, fact.clause);
      it->next();
    }
  }

  rts::FactSet new_buffer(facts.startingId());
  rts::Stacked<rts::Define> new_facts(&anchor, &new_buffer);
  rts::MutableSubstitution subst(facts.startingId(), facts.size());

  auto rename =
      rts::Predicate::Rename([&](Id id, Pid) { return subst.subst(id); });

  auto addFact = [&](Id id) {
    facts.factById(id, [&](Pid type, rts::Fact::Clause fact) {
      if (const auto* predicate = inventory.lookupPredicate(type)) {
        binary::Output out;
        uint64_t key_size;
        predicate->typecheck(rename, fact, out, key_size);
        const auto clause = rts::Fact::Clause::from(out.bytes(), key_size);
        auto new_id = new_facts.define(type, clause, Id::invalid());
        CHECK(new_id != Id::invalid());
        subst.set(id, new_id);
      } else {
        error("invalid predicate id {}", type);
      }
    });
  };

  // first add all the facts that are not owned
  {
    Id last = facts.startingId();
    auto it = owned.begin();
    if (it != owned.end()) {
      // up to the first owned fact, we can just copy because the substitution
      // is the identity
      for (auto id = last; id < it->lower(); id++) {
        facts.factById(id, [&](Pid type, rts::Fact::Clause fact) {
          auto new_id = new_facts.define(type, fact, Id::invalid());
          CHECK(new_id != Id::invalid());
          subst.set(id, new_id);
        });
      }
      last = it->upper() + 1;

      it++;
      for (; it != owned.end(); it++) {
        for (auto id = last; id < it->lower(); id++) {
          addFact(id);
        }
        last = it->upper() + 1;
      }
    }
    for (auto id = last; id < facts.firstFreeId(); id++) {
      addFact(id);
    }
  }

  // next add all the facts that are owned, so we have one interval
  auto owned_start = new_facts.firstFreeId();
  for (auto it = owned.begin(); it != owned.end(); it++) {
    for (auto id = it->lower(); id <= it->upper(); id++) {
      addFact(id);
    }
  }
  auto owned_end = new_facts.firstFreeId();

  rts::closed_interval_set<Id> res;
  res.add({owned_start, owned_end - 1});
  return std::make_pair(std::move(res), std::move(new_buffer));
}

void BatchBase::endUnit() {
  if (current) {
    current->finish = buffer.firstFreeId();
    if (current->finish > current->start) {
      auto p = minimalOwnership(
          inventory->inventory,
          buffer,
          anchor,
          current->start,
          current->finish);
      current->facts = std::move(p.first);
      buffer = std::move(p.second);
    }
    LOG(INFO) << current->unit << ": " << current->facts.size() << " / "
              << current->facts.iterative_size();
    current = nullptr;
    ++seen_units;

  } else {
    LOG(ERROR) << "mismatched endUnit";
  }
}

Id BatchBase::define(Pid ty, rts::Fact::Clause clause) {
  return facts.define(ty, clause);
}

void BatchBase::logEnd() const {
  LOG(INFO) << "saw " << seen_units << " units, serialized "
            << total_serialized_units;
}

} // namespace cpp
} // namespace glean
} // namespace facebook
