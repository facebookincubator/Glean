/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "glean/rts/factset.h"

namespace facebook {
namespace glean {
namespace rts {

struct FactSet::Index {
  /// Type of index maps
  using map_t = std::map<folly::ByteRange, const Fact*>;

  /// Type of index maps with synchronised access
  using entry_t = folly::Synchronized<map_t>;

  entry_t& operator[](Pid pid) {
    auto ulock = index.ulock();
    if (const auto p = ulock->lookup(pid)) {
      return **p;
    } else {
      auto wlock = ulock.moveFromUpgradeToWrite();
      auto q = new entry_t;
      (*wlock)[pid].reset(q);
      return *q;
    }
  }

  folly::Synchronized<DenseMap<Pid, std::unique_ptr<entry_t>>> index;
};

FactSet::FactSet(Id start) : facts(start) {}
FactSet::FactSet(FactSet&&) noexcept = default;
FactSet& FactSet::operator=(FactSet&&) = default;
FactSet::~FactSet() noexcept = default;

size_t FactSet::allocatedMemory() const noexcept {
  size_t n = facts.allocatedMemory() + keys.allocatedMemory();
  for (const auto& k : keys) {
    n += k.second.getAllocatedMemorySize();
  }
  return n;
}

size_t FactSet::Facts::allocatedMemory() const noexcept {
  size_t n = facts.capacity() * sizeof(facts[0]);
  for (const auto& fact : facts) {
    n += fact->size();
  }
  return n;
}

struct FactSet::CachedPredicateStats {
  struct Data {
    /// Cached stats
    PredicateStats stats;

    /// Index in 'facts' up to which we've computed the stats so far. Since
    /// FactSet is append-only, 'stats' are out of date if we have more than
    /// 'upto' facts and we only need to add the new facts to be up to date
    /// again
    size_t upto = 0;
  };
  folly::Synchronized<Data> data;
};

PredicateStats FactSet::predicateStats() const {
  auto ulock = predicate_stats.value().data.ulock();
  if (ulock->upto < facts.size()) {
    auto wlock = ulock.moveFromUpgradeToWrite();
    wlock->stats.reserve(keys.low_bound(), keys.high_bound());
    for (const auto& fact :
         folly::range(facts.begin() + wlock->upto, facts.end())) {
      wlock->stats[fact.type] += MemoryStats::one(fact.clause.size());
    }
    wlock->upto = facts.size();
    return wlock->stats;
  } else {
    return ulock->stats;
  }
}

Id FactSet::idByKey(Pid type, folly::ByteRange key) {
  if (const auto p = keys.lookup(type)) {
    const auto i = p->find(key);
    if (i != p->end()) {
      return (*i)->id();
    }
  }
  return Id::invalid();
}

Pid FactSet::typeById(Id id) {
  if (id >= facts.startingId()) {
    const auto i = distance(facts.startingId(), id);
    if (i < facts.size()) {
      return facts[i].type;
    }
  }
  return Pid::invalid();
}

bool FactSet::factById(Id id, std::function<void(Pid, Fact::Clause)> f) {
  if (id >= facts.startingId()) {
    const auto i = distance(facts.startingId(), id);
    if (i < facts.size()) {
      f(facts[i].type, facts[i].clause);
      return true;
    }
  }
  return false;
}

Interval FactSet::count(Pid pid) const {
  const auto p = keys.lookup(pid);
  return p ? p->size() : 0;
}

std::unique_ptr<FactIterator> FactSet::enumerate(Id from, Id upto) {
  struct Iterator final : FactIterator {
    using iter_t = FactSet::const_iterator;
    iter_t pos;
    const iter_t end;

    Iterator(iter_t p, iter_t e) : pos(p), end(e) {}

    void next() override {
      assert(pos != end);
      ++pos;
    }

    Fact::Ref get(Demand) override {
      return pos != end ? *pos : Fact::Ref::invalid();
    }

    std::optional<Id> lower_bound() override {
      return std::nullopt;
    }
    std::optional<Id> upper_bound() override {
      return std::nullopt;
    }
  };

  return std::make_unique<Iterator>(
      lower_bound(from), upto ? lower_bound(upto) : end());
}

std::unique_ptr<FactIterator> FactSet::enumerateBack(Id from, Id downto) {
  struct BackIterator final : FactIterator {
    using iter_t = FactSet::const_iterator;
    iter_t pos;
    const iter_t end;

    BackIterator(iter_t p, iter_t e) : pos(p), end(e) {}

    void next() override {
      assert(pos != end);
      --pos;
    }

    Fact::Ref get(Demand) override {
      if (pos != end) {
        auto i = pos;
        --i;
        return *i;
      } else {
        return Fact::Ref::invalid();
      }
    }

    std::optional<Id> lower_bound() override {
      return std::nullopt;
    }
    std::optional<Id> upper_bound() override {
      return std::nullopt;
    }
  };

  return std::make_unique<BackIterator>(
      from ? lower_bound(from) : end(), lower_bound(downto));
}

std::unique_ptr<FactIterator>
FactSet::seek(Pid type, folly::ByteRange start, size_t prefix_size) {
  struct SeekIterator : FactIterator {
    explicit SeekIterator(Index::map_t::iterator b, Index::map_t::iterator e)
        : current(b), end(e) {}

    void next() override {
      assert(current != end);
      ++current;
    }

    Fact::Ref get(Demand) override {
      return current != end ? current->second->ref() : Fact::Ref::invalid();
    }

    std::optional<Id> lower_bound() override {
      return std::nullopt;
    }
    std::optional<Id> upper_bound() override {
      return std::nullopt;
    }

    Index::map_t::const_iterator current;
    const Index::map_t::const_iterator end;
  };

  assert(prefix_size <= start.size());

  if (const auto p = keys.lookup(type)) {
    auto& entry = index.value()[type];
    // Check if the entry is up to date (i.e., has the same number of items as
    // the key hashmap). If it doesn't, fill it.
    if (!entry.withRLock([&](auto& map) { return map.size() == p->size(); })) {
      entry.withWLock([&](auto& map) {
        if (map.size() != p->size()) {
          map.clear();
          for (const Fact* fact : *p) {
            map.insert({fact->key(), fact});
          }
        }
      });
    }
    // The map for a pid is only created once so this is safe. We are *not*
    // thread safe with respect to concurrent modifications of the FactSet
    // as per spec.
    auto& map = entry.unsafeGetUnlocked();

    const auto next =
        binary::lexicographicallyNext({start.data(), prefix_size});
    return std::make_unique<SeekIterator>(
        map.lower_bound(start),
        next.empty() ? map.end() : map.lower_bound(binary::byteRange(next)));
  } else {
    return std::make_unique<EmptyIterator>();
  }
}

std::unique_ptr<FactIterator> FactSet::seekWithinSection(
    Pid type,
    folly::ByteRange start,
    size_t prefix_size,
    Id from,
    Id to) {
  if (from <= startingId() && firstFreeId() <= to) {
    return seek(type, start, prefix_size);
  }

  // We have no use case for actually performing a bounded
  // seek of a FactSet, therefore we would rather know if
  // anything tries to trigger it.
  error("FactSet::seekWithinSection: bounds too narrow");
}

Id FactSet::define(Pid type, Fact::Clause clause, Id) {
  if (clause.key_size > Fact::MAX_KEY_SIZE) {
    error("key too large: {}", clause.key_size);
  }
  const auto next_id = firstFreeId();
  auto fact = facts.alloc(next_id, type, clause);
  auto& key_map = keys[type];
  const auto r = key_map.insert(fact.get());
  if (r.second) {
    facts.commit(std::move(fact));
    return next_id;
  } else {
    return fact->value() == (*r.first)->value() ? (*r.first)->id()
                                                : Id::invalid();
  }
}

FactSet::Serialized FactSet::serialize() const {
  binary::Output output;
  for (auto fact : *this) {
    fact.serialize(output);
  }
  return {startingId(), size(), std::move(output)};
}

///
// Serialize facts in the order given by the input range. Preconditions:
//
// * The order must mention only fact IDs in this set.
// * The facts in the set cannot refer to each other (because then we
//   would need to substitute in addition to reordering)
//
// The ordering can omit facts. This is useful for filtering a FactSet,
// see Glean/Query/UserQuery.hs.
//
FactSet::Serialized FactSet::serializeReorder(
    folly::Range<const uint64_t*> order) const {
  binary::Output output;
  for (auto i : order) {
    assert(
        i >= startingId().toWord() && i - startingId().toWord() < facts.size());
    facts[i - startingId().toWord()].serialize(output);
  }

  return {startingId(), order.size(), std::move(output)};
}

namespace {

template <typename F>
std::pair<binary::Output, size_t> substituteFact(
    const Inventory& inventory,
    Predicate::Rename<F>& substitute,
    Fact::Ref fact) {
  auto predicate = inventory.lookupPredicate(fact.type);
  CHECK_NOTNULL(predicate);
  binary::Output clause;
  uint64_t key_size;
  predicate->substitute(substitute, fact.clause, clause, key_size);
  return {std::move(clause), key_size};
}

} // namespace

/*
  How rebasing works:

              |----------batch-----------|
   ...........|---global---|----local----|.......... ---> increasing fact IDs
             /              \             \
            /     subst      \             \
           /    maps these    \             \
          /                    \             \
   ......|----------------------|--new batch--|......
                        -->|    |<--
                           offset

  After applying a substitution to "batch", we have
    - global facts that are mapped by the substitution. We no longer
      have to keep those.
    - local facts that might now refer to global facts

  Local facts must be adjusted by "offset" so they don't overlap with
  global facts.
*/

std::pair<FactSet, Substitution> FactSet::rebase(
    const Inventory& inventory,
    const Substitution& subst,
    Store& global) const {
  const auto new_start = subst.firstFreeId();
  auto substitute =
      Predicate::Rename([&subst](Id id, Pid) { return subst.subst(id); });

  const auto split = lower_bound(subst.finish());

  for (auto fact : folly::range(begin(), split)) {
    auto r = substituteFact(inventory, substitute, fact);
    global.insert(
        {subst.subst(fact.id),
         fact.type,
         Fact::Clause::from(r.first.bytes(), r.second)});
  }

  // build a substitution that covers the whole of the FactSet,
  // by copying the original substitution into the lower part
  // and then filling the upper part as we add facts below.
  const auto subst_end = firstFreeId();

  std::vector<Id> rebase_ids(distance(subst.start(), subst_end));
  subst.with([&](Id base, const std::vector<Id>& items) {
    std::copy(items.begin(), items.end(), rebase_ids.begin());
  });
  MutableSubstitution localSubst(subst.start(), rebase_ids);

  auto substituteLocal = Predicate::Rename(
      [&localSubst](Id id, Pid) { return localSubst.subst(id); });

  FactSet local(new_start);
  for (auto fact : folly::range(split, end())) {
    auto r = substituteFact(inventory, substituteLocal, fact);

    const auto clause = Fact::Clause::from(r.first.bytes(), r.second);
    const auto old_id = fact.id;
    auto new_id = local.define(fact.type, clause);

    // in case of same key, but different values after substitution
    // we ignore the later definition.
    if (!new_id) {
      new_id = local.idByKey(fact.type, clause.key());
    }

    localSubst.set(old_id, new_id);
  }

  return std::make_pair<FactSet, Substitution>(
      std::move(local), localSubst.freeze());
}

void FactSet::append(FactSet other) {
  assert(appendable(other));

  facts.append(std::move(other.facts));

  keys.merge(std::move(other.keys), [](auto& left, const auto& right) {
    left.insert(right.begin(), right.end());
  });
}

bool FactSet::appendable(const FactSet& other) const {
  if (empty() || other.empty()) {
    return true;
  }

  if (firstFreeId() != other.startingId()) {
    return false;
  }

  for (const auto& k : other.keys) {
    if (const auto* p = keys.lookup(k.first)) {
      for (auto i = k.second.begin(); i != k.second.end(); ++i) {
        if (p->contains((*i)->key())) {
          return false;
        }
      }
    }
  }

  return true;
}

bool FactSet::sanityCheck() const {
  // TODO: implement
  return true;
}

} // namespace rts
} // namespace glean
} // namespace facebook
