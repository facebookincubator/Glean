/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <chrono>
#include <atomic>

#include <folly/Chrono.h>
#include <folly/stop_watch.h>
#include <thrift/lib/cpp2/protocol/Serializer.h>

#include "glean/if/gen-cpp2/glean_types.h"
#include "glean/if/gen-cpp2/glean_constants.h"
#include "glean/if/gen-cpp2/internal_types.h"
#include "glean/rts/query.h"

using namespace facebook::glean::thrift::internal;

namespace facebook {
namespace glean {
namespace rts {

namespace {

// check for timeout in 1% of all next() calls, because Clock::now()
// is not cheap.
static constexpr uint32_t CHECK_TIMEOUT_INTERVAL = 100;

using Clock = folly::chrono::coarse_steady_clock;

// all queries started before the last interrupt will be aborted.
std::atomic<std::chrono::time_point<Clock>> last_interrupt =
  folly::chrono::coarse_steady_clock::time_point::min();

struct QueryExecutor {

  // The following methods are all invoked from the compiled query
  // subroutine to access the DB and record results.

  // A live fact iterator
  using IterToken = uint64_t;

  //
  // Initiate a traversal of facts for a particular predicate and
  // key prefix. Returns a token that can be passed to next() to
  // fetch the next fact.
  //
  IterToken seek(Pid type, folly::ByteRange key);

  //
  // Returns the current seek token, so that the state can be reset in
  // the future using endSeek(). This is used for implementing
  // non-local jumps in the generated code.
  //
  IterToken currentSeek();

  //
  // Release the state associated with a previous seek() call.
  //
  void endSeek(IterToken token);

  //
  // Get the next fact in a traversal initiated by seek().  Returns 0
  // if there are no more facts.
  //
  Fact::Ref next(IterToken token, FactIterator::Demand demand);

  //
  // Look up a fact with id fid, and copy its key into kout and value
  // into vout.
  //
  // If kout is null, then don't copy the key (respectively vout/value).
  //
  // If kout is non-null, then it is reset to empty before copying the
  // key. This is to avoid the caller needing to remember to
  // ResetOutput() every time.
  //
  Pid lookupKeyValue(Id fid, binary::Output* kout, binary::Output* vout);

  //
  // Define a new derived fact, and return its fact ID.
  //
  Id newDerivedFact(Pid type, binary::Output* clause, size_t keySize);

  //
  // Save the current state of the execution in queryCont
  //
  void saveState(uint64_t* pc, uint64_t* frame);

  //
  // Record a nested fact that we visited during traversal, see
  // resultWithPid()
  //
  void nestedFact(Id id, Pid pid);
  Traverser nestedFact_{[this](Id id, Pid pid) { nestedFact(id,pid); }};

  //
  // Record a qeury result.
  //
  size_t resultWithPid(
      Id id,
      binary::Output *key,
      binary::Output *val,
      Pid pid,
      bool rec);

  //
  // wrapper around resultWithPid() used by ordinary queries where we
  // want the prevailing pid and recursive values.
  //
  // Result is:
  //   0   if this fact ID was already in the result set
  //   >0  if this fact ID is new (result is the size in bytes of the
  //       fact and all its recursively nested facts that were expanded)
  //
  size_t result(Id id, binary::Output* key, binary::Output* val) {
    auto added = results_added.insert(id.toWord());
    if (added.second) {
      return resultWithPid(id, key, val, pid, 0);
    } else {
      DVLOG(5) << "result skipped dup (" << id.toWord() << ")";
      return 0;
    }
  }

  //
  // Done; collect and return the final results
  //
  std::unique_ptr<QueryResults> finish();

  // ------------------------------------------------------------
  // Below here: query state

  // Timeouts
  folly::stop_watch<std::chrono::nanoseconds> watch;
  std::chrono::time_point<Clock> timeout;
  uint64_t check_timeout;

  bool timeExpired() {
    if (--check_timeout == 0) {
      if (Clock::now() > timeout) {
        return true;
      } else {
        check_timeout = CHECK_TIMEOUT_INTERVAL;
      }
    }
    return false;
  }

  std::chrono::time_point<Clock> start_time;
  inline bool interrupted() {
    auto last = last_interrupt.load(std::memory_order_relaxed);
    return last > start_time;
  }

  Inventory &inventory;
  Define &facts;
  DefineOwnership* ownership;
    // if null, don't compute ownership of derived facts
  Subroutine &sub;
  Pid pid;
  // expanding nested facts
  std::shared_ptr<Subroutine> traverse;
  Depth depth;
  std::unordered_set<Pid, folly::hasher<Pid>>& expandPids;

  // results so far
  folly::F14FastSet<uint64_t, folly::Hash> results_added;
  std::vector<uint64_t> result_ids;
  std::vector<uint64_t> result_pids;
  std::vector<HsString> result_keys;
  std::vector<HsString> result_values;

  // nested result facts
  folly::F14FastSet<uint64_t, folly::Hash> nested_results_added;
  std::vector<uint64_t> nested_result_ids;
  std::vector<uint64_t> nested_result_pids;
  std::vector<HsString> nested_result_keys;
  std::vector<HsString> nested_result_values;
  std::vector<Id> nested_result_pending;

  folly::Optional<thrift::internal::QueryCont> queryCont;

  // query stats
  folly::F14FastMap<uint64_t, uint64_t> stats;
  bool wantStats;

  // output registers
  std::vector<binary::Output> outputs;

  // iterators
  struct Iter {
    std::unique_ptr<rts::FactIterator> iter;
    // remember the type and current key so that we can capture the
    // state of this iterator for a continuation.
    Pid type;
    Id id;
    size_t prefix_size;
    bool first;
  };

  std::vector<Iter> iters;
};


uint64_t QueryExecutor::seek(Pid type, folly::ByteRange key) {
  auto token = iters.size();
  DVLOG(5) << "seek(" << type.toWord() << ") = " << token;
  iters.emplace_back(Iter{facts.seek(type, key, key.size()),
                          type, Id::invalid(), key.size(), true});
  return static_cast<uint64_t>(token);
};


uint64_t QueryExecutor::currentSeek() {
  return iters.size();
}

void QueryExecutor::endSeek(uint64_t token) {
  DVLOG(5) << "endSeek(" << token << ")";
  while (iters.size() > token) {
    iters.pop_back();
  }
};


Fact::Ref QueryExecutor::next(uint64_t token, FactIterator::Demand demand) {
  assert(token == iters.size()-1);
  if (iters[token].first) {
    iters[token].first = false;
  } else {
    iters[token].iter->next();
  }
  auto res = iters[token].iter->get(demand);
  if (res) {
    iters[token].id = res.id;
    if (wantStats) {
      stats[iters[token].type.toWord()]++;
    }
  }
  DVLOG(5) << "next(" << token << ") = " << (res ? res.id.toWord() : 0);
  return res;
};


Pid QueryExecutor::lookupKeyValue(
    Id fid,
    binary::Output* kout,
    binary::Output* vout) {
  DVLOG(5) << "lookupKeyValue(" << fid.toWord() << ")";
  Pid pid;
  facts.factById(fid, [&](Pid pid_, auto clause) {
    pid = pid_;
    if (kout) {
      *kout = binary::Output();
      kout->put(clause.key());
    }
    if (vout) {
      *vout = binary::Output();
      vout->put(clause.value());
    }
  });
  return pid;
};


Id QueryExecutor::newDerivedFact(
    Pid type,
    binary::Output* key,
    size_t keySize) {
  Fact::Clause clause = Fact::Clause::from(key->bytes(), keySize);
  auto id = facts.define(type, clause);

  // If we are going to store this derived fact in the DB, we need to
  // know its ownership set, which is determined by the facts it was
  // derived from.
  if (ownership) {
    std::set<UsetId> owners;

    // The Ids can only be facts that we already have computed owners for.
    for (const auto& iter : iters) {
      if (iter.id != Id::invalid()) {
        auto owner = ownership->getOwner(iter.id);
        if (owner == INVALID_USET) {
          VLOG(1) << "fact " << iter.id.toWord() << " has no owner";
        } else {
          owners.insert(owner);
        }
      }
    }
    if (owners.size() > 0) {
      ownership->derivedFrom(id, owners);
    }
  }

  return id;
};


void QueryExecutor::saveState(uint64_t *pc, uint64_t *frame) {
  thrift::internal::QueryCont cont;
  std::vector<thrift::internal::KeyIterator> contIters;

  for (auto &iter : iters) {
    thrift::internal::KeyIterator i;
    if (auto fact = iter.iter->get(FactIterator::KeyOnly)) {
      i.type() = iter.type.toWord();
      i.key() = binary::mkString(fact.key());
      i.prefix_size() = static_cast<int64_t>(iter.prefix_size);
      i.first() = iter.first;
    } else {
      // A finished iterator - we have no key to serialize
      i.type() = Pid::invalid().toWord();
    }
    contIters.emplace_back(std::move(i));
  }
  cont.iters() = std::move(contIters);

  std::vector<std::string> contOutputs;
  for (auto &output : outputs) {
    contOutputs.emplace_back(output.string());
  }
  cont.outputs() = std::move(contOutputs);

  thrift::internal::SubroutineState subState;
  subState.code() =
      std::string(reinterpret_cast<const char *>(sub.code.data()),
                  sub.code.size() * sizeof(uint64_t));
  subState.entry() = pc - sub.code.data();
  subState.literals() = sub.literals;
  std::vector<int64_t> locals(sub.locals);
  std::copy(frame + sub.inputs, frame + sub.inputs + sub.locals, locals.data());
  subState.locals() = std::move(locals);
  subState.inputs() = sub.inputs;
  cont.sub() = std::move(subState);
  cont.pid() = pid.toWord();
  if (traverse) {
    cont.traverse() = Subroutine::toThrift(*traverse);
  }
  queryCont = std::move(cont);
};


void QueryExecutor::nestedFact(Id id, Pid pid) {
  DVLOG(5) << "nestedFact: " << id.toWord();
  if (depth == Depth::ExpandPartial &&
      expandPids.find(pid) == expandPids.end()) {
    return;
  }
  auto added = nested_results_added.insert(id.toWord());
  if (added.second) {
    nested_result_pending.emplace_back(id);
  }
};


size_t QueryExecutor::resultWithPid(
    Id id,
    binary::Output *key,
    binary::Output *val,
    Pid pid,
    bool rec) {
  assert(id != Id::invalid());
  result_ids.emplace_back(id.toWord());
  result_pids.emplace_back(pid.toWord());
  result_keys.emplace_back(key ? key->string() : "");
  result_values.emplace_back(val ? val->string() : "");
  DVLOG(5) << "result added (" << id.toWord() << ")";
  auto key_size = key ? key->size() : 0;
  auto val_size = val ? val->size() : 0;
  size_t bytes = sizeof(Id) + key_size + val_size;
  if (rec || depth != Depth::ResultsOnly) {
    {
      binary::Output bin;
      bin.expect(key_size + val_size);
      if (key) {
        bin.put(key->bytes());
      }
      if (val) {
        bin.put(val->bytes());
      }
      auto clause = Fact::Clause::from(bin.bytes(), key_size);
      if (traverse) {
        Predicate::runTraverse(*traverse, nestedFact_, clause);
      } else {
        auto predicate = inventory.lookupPredicate(pid);
        if (!predicate) {
          error("unknown pid: {}", pid.toWord());
        }
        predicate->traverse(nestedFact_, clause);
      }
    }
    while (nested_result_pending.size() > 0) {
      auto id = nested_result_pending[nested_result_pending.size() - 1];
      nested_result_pending.pop_back();
      facts.factById(id, [&](Pid pid_, auto clause) {
        inventory.lookupPredicate(pid_)->traverse(nestedFact_, clause);
        nested_result_ids.emplace_back(id.toWord());
        nested_result_pids.emplace_back(pid_.toWord());
        auto key = binary::mkString(clause.key());
        auto val = binary::mkString(clause.value());
        bytes += sizeof(Id) + key.size() + val.size();
        nested_result_keys.emplace_back(std::move(key));
        nested_result_values.emplace_back(std::move(val));
      });
    }
  }
  return bytes;
};


std::unique_ptr<QueryResults> QueryExecutor::finish() {
  auto res = std::make_unique<QueryResults>();
  res->fact_ids = std::move(result_ids);
  res->fact_pids = std::move(result_pids);
  res->fact_keys = std::move(result_keys);
  res->fact_values = std::move(result_values);
  res->nested_fact_ids = std::move(nested_result_ids);
  res->nested_fact_pids = std::move(nested_result_pids);
  res->nested_fact_keys = std::move(nested_result_keys);
  res->nested_fact_values = std::move(nested_result_values);

  if (queryCont) {
    std::string out;
    using namespace apache::thrift;
    Serializer<BinaryProtocolReader, BinaryProtocolWriter>::serialize(
        *queryCont, &out);
    res->continuation = std::move(out);
  };

  if (wantStats) {
    res->stats = std::move(stats);
  }
  res->elapsed_ns = watch.elapsed().count();
  return res;
}

} // namespace {}

void interruptRunningQueries() {
  last_interrupt = Clock::now();
}

std::unique_ptr<QueryResults> restartQuery(
    Inventory& inventory,
    Define& facts,
    DefineOwnership* ownership,
    folly::Optional<uint64_t> maxResults,
    folly::Optional<uint64_t> maxBytes,
    folly::Optional<uint64_t> maxTime,
    Depth depth,
    std::unordered_set<Pid, folly::hasher<Pid>>& expandPids,
    bool wantStats,
    void* serializedCont,
    uint64_t serializedContLen) {
  thrift::internal::QueryCont queryCont;

  // Deserialize the continuation into thrift::internal::QueryCont
  using namespace apache::thrift;
  Serializer<BinaryProtocolReader, BinaryProtocolWriter>::deserialize(
      folly::ByteRange(
          reinterpret_cast<unsigned char*>(serializedCont), serializedContLen),
      queryCont);

  // Build a Subroutine
  uint64_t* code = reinterpret_cast<uint64_t*>(
    const_cast<char*>(queryCont.sub()->code()->data()));
  auto code_size = queryCont.sub()->code()->size() / sizeof(uint64_t);
  Subroutine sub{std::vector<uint64_t>(code, code + code_size),
                 static_cast<size_t>(*queryCont.sub()->inputs()),
                 queryCont.outputs()->size(),
                 static_cast<size_t>(queryCont.sub()->locals()->size()),
                 {}, // no constants - they're already on the stack
                 std::move(*queryCont.sub()->literals())};

  std::shared_ptr<Subroutine> traverse;
  if (queryCont.traverse().has_value()) {
    traverse = Subroutine::fromThrift(*queryCont.traverse());
  }

  // Setup the state as it was before, and execute the Subroutine
  auto pid = Pid::fromWord(*queryCont.pid());

  return executeQuery(
      inventory,
      facts,
      ownership,
      sub,
      pid,
      traverse,
      maxResults,
      maxBytes,
      maxTime,
      depth,
      expandPids,
      wantStats,
      std::move(queryCont));
}


std::unique_ptr<QueryResults> executeQuery (
    Inventory& inventory,
    Define& facts,
    DefineOwnership* ownership,
    Subroutine& sub,
    Pid pid,
    std::shared_ptr<Subroutine> traverse,
    folly::Optional<uint64_t> maxResults,
    folly::Optional<uint64_t> maxBytes,
    folly::Optional<uint64_t> maxTime,
    Depth depth,
    std::unordered_set<Pid, folly::hasher<Pid>>& expandPids,
    bool wantStats,
    folly::Optional<thrift::internal::QueryCont> restart) {

  QueryExecutor q {
    .inventory = inventory,
    .facts = facts,
    .ownership = ownership,
    .sub = sub,
    .pid = pid,
    .traverse = traverse,
    .depth = depth,
    .expandPids = expandPids,
    .wantStats = wantStats
  };
  // coarse_steady_clock is around 1ms granularity which is enough for us.
  q.timeout = Clock::now();
  q.start_time = Clock::now();
  if (maxTime) {
    q.timeout += std::chrono::milliseconds{*maxTime};
    q.check_timeout = CHECK_TIMEOUT_INTERVAL;
  } else {
    q.check_timeout = UINT64_MAX;
  }

  q.outputs.resize(sub.outputs);

  // Set up all the iterators as before if we're restarting
  if (restart) {
    for (auto& savedIter : *restart->iters()) {
      std::unique_ptr<FactIterator> iter;
      Id id;
      if (const auto type = Pid::fromThrift(*savedIter.type())) {
        auto key = binary::byteRange(*savedIter.key());
        iter = facts.seek(type, key, savedIter.get_prefix_size());
        auto res = iter->get(FactIterator::KeyOnly);
        if (!res || res.key() != key) {
          error("restart iter didn't find a key");
        }
        id = res.id;
      } else {
        // We serialized a finished iterator
        iter = std::make_unique<EmptyIterator>();
        id = Id::invalid();
      }
      q.iters.emplace_back(QueryExecutor::Iter{std::move(iter),
          Pid::fromWord(*savedIter.type()),
          id,
          static_cast<size_t>(savedIter.get_prefix_size()),
          *savedIter.first()});
    }
  }

  if (restart) {
    for (auto i = 0; i < sub.outputs; i++) {
      q.outputs[i].bytes(
          restart->outputs()[i].data(), restart->outputs()[i].size());
    }
  }

  auto max_results = maxResults ? *maxResults : UINT64_MAX;
  auto max_bytes = maxBytes ? *maxBytes : UINT64_MAX;

  // IF YOU BREAK BACKWARD COMPATIBILITY HERE, BUMP version IN
  // Glean.Bytecode.Generate.Instruction
  //
  // IF YOU ALSO BREAK FORWARD COMPATIBILITY, BUMP latestSupportedVersion AS
  // WELL

  const std::function<uint64_t(uint64_t, uint64_t, uint64_t)> seek_ =
      [&](uint64_t type, uint64_t prefix, uint64_t end) {
        return q.seek(
            Pid::fromWord(type),
            folly::ByteRange(
                reinterpret_cast<uint8_t *>(prefix),
                reinterpret_cast<uint8_t *>(end)));
      };


  const std::function<uint64_t()> currentSeek_ = [&]() -> uint64_t {
    return q.currentSeek();
  };

  const std::function<void(uint64_t)> endSeek_ = [&](uint64_t token) {
    q.endSeek(token);
  };

  const std::function<uint64_t(uint64_t, uint64_t, uint64_t *, uint64_t *,
                               uint64_t *, uint64_t *)>
      next_ = [&](uint64_t token, uint64_t demand, uint64_t *clause_begin,
                  uint64_t *key_end, uint64_t *clause_end, uint64_t *id) {
        if (q.timeExpired()) {
          return 2;
        }
        if (q.interrupted()) {
          return 2;
        }
        auto res = q.next(token, demand != 0 ? FactIterator::KeyValue
                                             : FactIterator::KeyOnly);
        if (!res) {
          return 0;
        }
        *id = res.id.toWord();
        *clause_begin = reinterpret_cast<uint64_t>(res.clause.bytes().data());
        *key_end = reinterpret_cast<uint64_t>(res.clause.key().end());
        *clause_end = reinterpret_cast<uint64_t>(res.clause.bytes().end());
        return 1;
      };

  const std::function<uint64_t(uint64_t, uint64_t *, uint64_t *)>
      lookupKeyValue_ = [&](uint64_t fid, uint64_t *kout, uint64_t *vout) {
        return q.lookupKeyValue(
            Id::fromWord(fid),
            reinterpret_cast<binary::Output *>(kout),
            reinterpret_cast<binary::Output *>(vout)).toWord();
      };

  const std::function<uint64_t(uint64_t, uint64_t *, uint64_t)>
      newDerivedFact_ = [&](uint64_t type, uint64_t *key, uint64_t size) {
        return q.newDerivedFact(
            Pid::fromWord(type),
            reinterpret_cast<binary::Output *>(key),
            size).toWord();
      };

  const std::function<void(uint64_t *, uint64_t *)> saveState_ =
      [&](uint64_t *pc, uint64_t *frame) { q.saveState(pc, frame); };

  const std::function<void(uint64_t, binary::Output *, binary::Output *,
                           uint64_t, uint64_t)>
      resultWithPid_ = [&](uint64_t id, binary::Output *key,
                           binary::Output *val, uint64_t pid, uint64_t rec) {
        q.resultWithPid(
            Id::fromWord(id),
            key,
            val,
            Pid::fromWord(pid),
            rec);
      };

  const std::function<uint64_t(uint64_t, binary::Output *, binary::Output *)>
      result_ = [&](uint64_t id, binary::Output *key, binary::Output *val) {
        return q.result(Id::fromWord(id), key, val);
      };

  std::vector<uint64_t> args;

  if (restart) {
    args.reserve(sub.inputs + sub.locals);
  } else {
    args.reserve(sub.inputs);
  }

  args.push_back(reinterpret_cast<uint64_t>(&seek_));
  args.push_back(reinterpret_cast<uint64_t>(&currentSeek_));
  args.push_back(reinterpret_cast<uint64_t>(&endSeek_));
  args.push_back(reinterpret_cast<uint64_t>(&next_));
  args.push_back(reinterpret_cast<uint64_t>(&lookupKeyValue_));
  args.push_back(reinterpret_cast<uint64_t>(&result_));
  args.push_back(reinterpret_cast<uint64_t>(&resultWithPid_));
  args.push_back(reinterpret_cast<uint64_t>(&newDerivedFact_));
  args.push_back(reinterpret_cast<uint64_t>(&saveState_));
  args.push_back(reinterpret_cast<uint64_t>(max_results));
  args.push_back(reinterpret_cast<uint64_t>(max_bytes));
  for (auto i = 0; i < sub.outputs; i++) {
    args.push_back(reinterpret_cast<uint64_t>(&q.outputs[i]));
  }

  if (restart) {
    std::copy(
        restart->sub()->locals()->begin(),
        restart->sub()->locals()->end(),
        std::back_inserter(args));
    sub.restart(args.data(), *restart->sub()->entry());
  } else {
    sub.execute(args.data());
  }

  return q.finish();
}


}
}
}
