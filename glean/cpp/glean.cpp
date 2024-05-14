/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <folly/gen/Base.h>
#include <iterator>
#include <algorithm>
#include "glean/cpp/glean.h"
#include "glean/rts/sanity.h"

namespace facebook {
namespace glean {
namespace cpp {

BatchBase::BatchBase(const SchemaInventory *inv, size_t cache_capacity)
  : inventory(inv)
  , stats(std::make_shared<rts::LookupCache::Stats>())
  , cache(
      rts::LookupCache::Options{
        cache_capacity,
        0 // disable deferred LRU list manipulation
      },
      stats)
  , anchor(&rts::EmptyLookup::instance(), &cache)
  , buffer(Id::lowest())
  , facts(&anchor, &buffer)
{}

rts::FactSet::Serialized BatchBase::serialize() const {
  return buffer.serialize();
}

std::map<std::string, std::vector<int64_t>> BatchBase::serializeOwnership() const {
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
    auto r  = buffer.rebase(inventory->inventory, subst, store);
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
    values[rts::LookupCache::Stats::factCount]
  };
  res.hits = values[rts::LookupCache::Stats::idByKey_hits]
    + values[rts::LookupCache::Stats::typeById_hits]
    + values[rts::LookupCache::Stats::factById_hits];
  res.misses = values[rts::LookupCache::Stats::idByKey_misses]
    + values[rts::LookupCache::Stats::idByKey_failures]
    + values[rts::LookupCache::Stats::typeById_misses]
    + values[rts::LookupCache::Stats::typeById_failures]
    + values[rts::LookupCache::Stats::factById_misses]
    + values[rts::LookupCache::Stats::factById_failures];
  return res;
}

void BatchBase::beginUnit(std::string unit) {
  owned.push_back({std::move(unit), {}, buffer.firstFreeId(), Id::invalid()});
  current = &owned.back();
}

void BatchBase::endUnit() {
  if (current) {
    LOG(INFO) << current->unit << ": " << current->facts.size() << " / " << current->facts.iterative_size();
    current->finish = buffer.firstFreeId();
    current = nullptr;
    ++seen_units;
  } else {
    LOG(ERROR) << "mismatched endUnit";
  }
}

Id BatchBase::define(Pid ty, rts::Fact::Clause clause) {
  const auto id = facts.define(ty, clause);
  if (id) {
    if (current != nullptr) {
      current->facts.add(id);
    }
  }
  return id;
}

void BatchBase::logEnd() const {
  LOG(INFO) << "saw " << seen_units << " units, serialized " << total_serialized_units;
}

}
}
}
