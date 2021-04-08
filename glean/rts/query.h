#pragma once

#include <folly/container/F14Map.h>

#include "glean/rts/factset.h"
#include "glean/rts/inventory.h"
#include "glean/if/gen-cpp2/internal_types.h"

#ifdef OSS
#include <cpp/HsStruct.h>
#else
#include <common/hs/util/cpp/HsStruct.h>
#endif

namespace facebook {
namespace glean {
namespace rts {

struct QueryResults {
  HsArray<uint64_t> fact_ids;
  HsArray<uint64_t> fact_pids;
  HsArray<HsString> fact_keys;
  HsArray<HsString> fact_values;
  HsArray<uint64_t> nested_fact_ids;
  HsArray<uint64_t> nested_fact_pids;
  HsArray<HsString> nested_fact_keys;
  HsArray<HsString> nested_fact_values;
  HsMap<uint64_t, uint64_t> stats;
  uint64_t elapsed_ns;
  HsString continuation;
};

enum class Depth {
  ResultsOnly,
  ExpandRecursive,
  ExpandPartial
};

QueryResults executeQuery(
    Inventory& inventory,
    Define& facts,
    Subroutine& sub,
    Pid pid,
    std::shared_ptr<Subroutine> traverse,
    folly::Optional<uint64_t> maxResults,
    folly::Optional<uint64_t> maxBytes,
    folly::Optional<uint64_t> maxTime,
    Depth depth,
    std::unordered_set<Pid, folly::hasher<Pid>>& expandPids,
    bool wantStats,
    folly::Optional<thrift::internal::QueryCont> restart);

QueryResults restartQuery(
    Inventory& inventory,
    Define& facts,
    folly::Optional<uint64_t> maxResults,
    folly::Optional<uint64_t> maxBytes,
    folly::Optional<uint64_t> maxTime,
    Depth depth,
    std::unordered_set<Pid, folly::hasher<Pid>>& expandPids,
    bool wantStats,
    void* serializedCont,
    uint64_t serializedContLen);

void interruptRunningQueries();
}
}
}
