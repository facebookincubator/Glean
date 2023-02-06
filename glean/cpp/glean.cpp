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

thrift::Batch BatchBase::serialize() const {
  thrift::Batch batch;
  auto s = buffer.serialize();
  batch.firstId() = s.first.toThrift();
  batch.count() = s.count;
  batch.facts() = s.facts.moveToFbString();
  return batch;
}

void BatchBase::rebase(const thrift::Subst& s) {
  std::vector<Id> ids(s.ids().value().size());
  std::transform(
      s.ids().value().begin(),
      s.ids().value().end(),
      ids.begin(),
      Id::fromWord);
  auto subst =
      rts::Substitution(Id::fromWord(s.firstId().value()), std::move(ids));
  GLEAN_SANITY_CHECK(subst.sanityCheck(false));
  cache.withBulkStore([&](auto& store) {
    buffer = buffer.rebase(inventory->inventory, subst, store);
    facts = rts::Stacked<rts::Define>(&anchor, &buffer);
  });
  GLEAN_SANITY_CHECK(buffer.sanityCheck());
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

}
}
}
