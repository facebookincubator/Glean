#include <folly/gen/Base.h>
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
  return buffer.serialize();
}

void BatchBase::rebase(const thrift::Subst& s) {
  auto subst = rts::Substitution::deserialize(s);
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
