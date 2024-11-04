/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <sys/types.h>
#include <cstdint>
#include <optional>
#ifdef OSS
#include <cpp/wrap.h> // @manual
#else
#include <common/hs/util/cpp/wrap.h>
#endif
#include <folly/MPMCQueue.h>
#include <folly/Synchronized.h>
#include <folly/Unit.h>
#include <folly/executors/GlobalExecutor.h>
#include <folly/futures/Future.h>
#include "glean/rts/binary.h"
#include "glean/rts/inventory.h"
#include "glean/rts/lookup.h"
#include "glean/rts/stacked.h"

#include <atomic>
#include <future>
#include <iostream>
#include <list>
#include <queue>
#include <vector>

using namespace facebook::hs;
using namespace facebook::glean;
using namespace facebook::glean::rts;

namespace {

// A group of facts to be deduplicated together. In the final stage we must
// process these in order, therefore they need a sequence number.
struct Batch {
  size_t sequence_number;
  std::vector<std::shared_ptr<Fact>> facts;
};

// A queue supporting multiple producers and consumers.
using Queue = folly::MPMCQueue<std::optional<Batch>, std::atomic, false>;

// Write to a queue. Blocking.
void write(Queue& q, std::optional<Batch> batch) {
  auto wait = std::chrono::seconds(60);
  auto soon = std::chrono::system_clock::now() + wait;
  bool written = q.tryWriteUntil(soon, std::move(batch));
  if (!written) {
    error("unable to write");
  }
}

void terminate_consumers(Queue& q) {
  write(q, std::nullopt);
}

// Read from a queue until it finishes.
void consume(Queue& q, std::function<void(Batch)> fn) {
  std::optional<Batch> input;
  q.blockingRead(input);
  while (input != std::nullopt) {
    fn(*input);
    q.blockingRead(input);
  }
  // leave the sentinel value in the queue to stop other threads.
  terminate_consumers(q);
}

// An efficient thread-safe substitution for blocks of consecutive ids.
// We protect substitution sections with mutexes.
size_t max_shard_size = 10000;
struct TSubstitution {
 public:
  TSubstitution(Id first, size_t size_)
      : size(size_), base(first), shard_size(std::min(size, max_shard_size)) {
    auto shard_count =
        size / shard_size + std::min(1, (int)(size % shard_size));
    shards.reserve(shard_count);
    for (size_t ix = 0; ix < shard_count; ix++) {
      auto size_this_shard = std::min(shard_size, size - (ix * shard_size));
      shards.emplace_back(Shard{{size_this_shard, std::nullopt}, 0});
    }
  }

  Id start() const {
    return base;
  }
  Id finish() const {
    return base + size;
  }
  std::optional<Id> subst(const Id id) {
    if (id >= start() && id < finish()) {
      const auto d = distance(start(), id);
      const auto shard_index = d / shard_size;
      const auto item_index = d % shard_size;
      return shards.at(shard_index).withRLock([&](const Shard& shard) {
        return shard.items.at(item_index);
      });
    } else {
      return Id::invalid();
    }
  }

  void set(const Id pos, const Id id) {
    CHECK(pos >= start() && pos < finish());
    const size_t d = distance(start(), pos);
    const auto shard_index = d / shard_size;
    const auto item_index = d % shard_size;
    shards.at(shard_index).withWLock([&](Shard& shard) {
      shard.items.at(item_index) = id;
      if (id != Id::invalid()) {
        shard.used++;
      }
    });
  }

  size_t mapped() {
    auto total = 0;
    for (auto& shard : shards) {
      shard.withRLock([&](const Shard& shard) { total += shard.used; });
    }
    return total;
  }

 private:
  struct Shard {
    std::vector<std::optional<Id>> items;
    size_t used = 0; // values mapped to a valid Id.
  };

  const size_t size;
  const Id base;
  const size_t shard_size;
  std::vector<folly::Synchronized<Shard>> shards;
};

} // namespace

extern "C" {

// To ensure a fact is not in the database we must query for it after having
// considered all facts that came before it. This is naturally sequential. To
// decrease the bottleneck of the final dedupe we try to dedupe as much as
// possible in parallel before the final deduplication.
//
// We don't want very large deduping batches or too many parallel deduplication
// threads as each fact in one of those threads may not be adequately renamed
// if it makes references to facts that are still in the other threads, taking
// it to the final deduplication step.
//
// With efficient deduplication we get bottlenecked on the read side, so we
// also add parallel reading threads.
//
//
//  read ┐                    ┌ dedupe ┐
//       │                    │        │
//       │  queue_undeduped   │        │  queue_deduped
//  read ┿──────────────────> ┿ dedupe ┿────────────────> final dedupe
//       │                    │        │
//       │                    │        │
//  read ┘                    └ dedupe ┘
//
//
const char* glean_diff(
    size_t pids_count,
    int64_t pids_lowest,
    const int64_t* pids_first,
    bool pids_mismatch,
    Inventory* second_inventory,
    Lookup* first_lookup,
    Lookup* second_lookup,
    bool log_added,
    size_t batch_size, // how many facts to deduplicate together
    size_t* kept_,
    size_t* added_,
    size_t* removed_) {
  return ffi::wrap([=] {
    const auto first_starting = first_lookup->startingId();
    const auto first_boundary = first_lookup->firstFreeId();
    const auto second_starting = second_lookup->startingId();
    const auto second_boundary = second_lookup->firstFreeId();

    const auto second_size = distance(second_starting, second_boundary);
    const auto first_size = distance(first_starting, first_boundary);
    TSubstitution subst(second_starting, second_size);

    const auto parallel_dedupes = 10;
    const auto parallel_reads = 10;

    // The queue_undeduped queue can be as large as we want, but must not be too
    // small otherwise dedupers might lay idle.
    Queue queue_undeduped(2 * parallel_dedupes);

    // We want the writer to always have things to deduplicate but the larger
    // this queue the less useful are the susbstituions that we have. So we want
    // to keep it as low as possible without ever allowing the writer to be
    // blocked.
    Queue queue_deduped(2 * parallel_dedupes);

    enum class SearchResult { Found, NotFound, Invalid };

    const auto toFirst = [&](const Pid pid_second) {
      if (pids_mismatch) {
        auto ix = pid_second.toWord() - pids_lowest;
        assert(ix >= 0 && ix < pids_count);
        return Pid::fromWord(pids_first[ix]);
      } else {
        return pid_second;
      }
    };

    // Find a fact from 'second' in 'first' and save the id mapping if found. If
    // we don't have mappings for some of the referenced ids, they get mapped to
    // Id::invalid. This guarantees that we won't find any match when searching
    // in the DB.
    const auto find = [&](const std::shared_ptr<Fact>& fact_second) {
      auto pid_second = fact_second->type();
      auto pid_first = toFirst(pid_second);
      if (pid_first == Pid::invalid()) {
        return SearchResult::Invalid;
      }

      auto pred = second_inventory->lookupPredicate(pid_second);
      CHECK(pred != nullptr);
      binary::Output out;
      uint64_t key_size;

      bool has_missing_reference = false;
      bool has_invalid_reference = false;
      auto substitute = Predicate::Rename([&](Id id, Pid) {
        if (has_invalid_reference || has_missing_reference) {
          return Id::invalid();
        }
        auto found = subst.subst(id);
        if (found) {
          has_invalid_reference = *found == Id::invalid();
          return *found;
        } else {
          has_missing_reference = true;
          return Id::invalid();
        }
      });

      pred->substitute(substitute, fact_second->clause(), out, key_size);
      if (has_invalid_reference) {
        return SearchResult::Invalid;
      }
      if (has_missing_reference) {
        return SearchResult::NotFound;
      }

      // clause from 'second' with translated ids.
      Fact::Clause clause_second = Fact::Clause::from(out.bytes(), key_size);

      // assumes that both lookups use the same Pids.
      Id id = first_lookup->idByKey(pid_first, clause_second.key());

      bool found = false;
      if (id != Id::invalid()) {
        if (clause_second.value_size == 0) {
          found = true;
        } else {
          first_lookup->factById(id, [&](Pid, Fact::Clause clause_first) {
            found = clause_second.value() == clause_first.value();
          });
        }
      }

      // At this point the fact has no missing or invalid references so the
      // search result will be final.
      subst.set(fact_second->id(), id);
      if (found) {
        return SearchResult::Found;
      } else {
        if (log_added) {
          std::cout << "Added $" << fact_second->id().toWord() << ": "
                    << pred->name << std::endl;
        }
        return SearchResult::Invalid;
      }
    };

    const auto dedupe = [&](Batch batch) {
      std::vector<std::shared_ptr<Fact>> not_found{};
      for (auto const& fact : batch.facts) {
        SearchResult found = find(fact);

        if (found == SearchResult::NotFound) {
          not_found.push_back(fact);
        }
      }

      return Batch{batch.sequence_number, not_found};
    };

    auto executor = folly::getGlobalCPUExecutor();
    // start parallel deduplication
    folly::Future<folly::Unit> initial_dedupe = folly::via(executor, [&]() {
      std::vector<folly::Future<folly::Unit>> workers{};
      for (auto i = 0; i < parallel_dedupes; i++) {
        workers.push_back(folly::via(executor, [&]() {
          consume(queue_undeduped, [&](Batch batch) {
            write(queue_deduped, dedupe(batch));
          });
          return folly::unit;
        }));
      }

      // wait for deduplication to finish
      folly::collect(std::move(workers)).get();
      terminate_consumers(queue_deduped);
      return folly::unit;
    });

    // start final single-threaded deduplication
    folly::Future<folly::Unit> final_dedupe = folly::via(executor, [&]() {
      size_t one_percent =
          std::max((size_t)1, second_size / (100 * batch_size));
      size_t announce = one_percent; // announce on every one percent of batches
      auto cmp = [](const Batch& one, const Batch& two) {
        return one.sequence_number > two.sequence_number;
      };
      // min_heap
      std::priority_queue<Batch, std::vector<Batch>, decltype(cmp)> queued{};

      size_t next_index = 0;
      size_t max_queue_size = 0;
      consume(queue_deduped, [&](Batch batch) {
        queued.push(std::move(batch));
        max_queue_size = std::max(max_queue_size, queued.size());
        while (!queued.empty() && queued.top().sequence_number == next_index) {
          auto next = queued.top();
          dedupe(next);
          queued.pop();
          next_index++;
          if (--announce == 0) {
            announce = one_percent;
            auto percentage = next.sequence_number / one_percent;
            std::cerr << percentage << "%" << std::endl << std::flush;
          }
        }
      });
      return folly::unit;
    });

    const size_t batches_per_group = 10;
    using Group = std::vector<Batch>;
    auto read_batch_group = [&](const size_t batch_index) {
      Group group{};

      const Id start = second_lookup->startingId() +
          batch_index * batches_per_group * batch_size;
      if (start >= second_lookup->firstFreeId()) {
        return group;
      }

      const Id end = start + batch_size * batches_per_group;
      auto i = second_lookup->enumerate(start, end);
      auto ref = i->get();
      while (ref && group.size() < batches_per_group) {
        const auto sequence_number =
            batch_index * batches_per_group + group.size();
        Batch batch{sequence_number, {}};
        batch.facts.reserve(batch_size);
        while (ref && batch.facts.size() < batch_size) {
          batch.facts.push_back(Fact::create(ref));
          i->next();
          ref = i->get();
        }
        group.push_back(batch);
      }
      return group;
    };

    // we start parallel_reads threads initially and start another read whenever
    // the oldest one of these threads finishes pushing all of its batches.
    folly::Future<folly::Unit> read_db = folly::via(executor, [&]() {
      std::atomic<int> next_batch = 0;
      std::list<folly::Future<Group>> read_queue{};

      for (auto i = 0; i < parallel_reads; i++) {
        read_queue.push_back(folly::via(executor, [&]() {
          auto next = next_batch++;
          return read_batch_group(next);
        }));
      }

      while (read_queue.size()) {
        Group group = read_queue.front().via(executor).get();
        read_queue.pop_front();
        bool finished_reading = group.size() < batches_per_group;
        if (!finished_reading) {
          read_queue.push_back(folly::via(executor, [&]() {
            auto next = next_batch++;
            return read_batch_group(next);
          }));
        }
        for (auto& batch : group) {
          write(queue_undeduped, batch);
        }
      }

      terminate_consumers(queue_undeduped);
      return folly::unit;
    });

    folly::collect(read_db, initial_dedupe, final_dedupe).get();

    size_t kept = subst.mapped();
    *kept_ = kept;
    *added_ = second_size - kept;
    // removed can be incorrect if the first db is incremental as it includes
    // facts in excluded units.
    *removed_ = first_size - kept;
  });
}
}
