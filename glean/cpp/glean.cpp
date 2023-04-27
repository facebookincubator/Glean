/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <folly/gen/Base.h>
#include <folly/FBString.h>
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
  if(ow.size() != owned.size()) {
    LOG(ERROR) << "unexpected number of serialized units "
      << ow.size() << " vs. " << owned.size();
    std::vector<std::string> us;
    for (const auto& p : owned) {
      us.push_back(p.unit);
    }
    std::sort(us.begin(), us.end());
    std::string buf;
    for (const auto& s : us) {
      buf += s;
      buf += ' ';
    }
    LOG(FATAL) << buf;
  }
  last_serialized_units = owned.size();
  total_serialized_units += owned.size();
  return ow;
}

void BatchBase::rebase(const rts::Substitution& subst) {
  const auto boundary = subst.finish();
  GLEAN_SANITY_CHECK(subst.sanityCheck(false));
  cache.withBulkStore([&](auto& store) {
    buffer = buffer.rebase(inventory->inventory, subst, store);
    facts = rts::Stacked<rts::Define>(&anchor, &buffer);
  });
  GLEAN_SANITY_CHECK(buffer.sanityCheck());
  auto p = std::find_if(
    owned.begin(),
    owned.end(),
    [boundary](const auto& p) { return p.finish > boundary; }
  );
  if (p - owned.begin() != last_serialized_units) {
    LOG(ERROR) << "serialized " << last_serialized_units << " but rebasing "
      << (p - owned.begin());
  }
  for (auto i = p; i != owned.end(); ++i) {
    i->facts = subst.rebaseIntervals(i->facts);
  }
  owned.erase(owned.begin(), p);
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
    } else {
      LOG(WARNING) << "define outside unit";
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
