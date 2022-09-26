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
  using map_t = std::map<folly::ByteRange, const Fact *>;

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

FactSet::FactSet(Id start) : starting_id(start), fact_memory(0) {}
FactSet::FactSet(FactSet&&) noexcept = default;
FactSet& FactSet::operator=(FactSet&&) = default;
FactSet::~FactSet() noexcept = default;

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
  if (id >= starting_id) {
    const auto i = distance(starting_id, id);
    if (i < facts.size()) {
      return facts[i]->type();
    }
  }
  return Pid::invalid();
}

bool FactSet::factById(Id id, std::function<void(Pid, Fact::Clause)> f) {
  if (id >= starting_id) {
    const auto i = distance(starting_id, id);
    if (i < facts.size()) {
      f(facts[i]->type(), facts[i]->clause());
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

    Iterator(iter_t p, iter_t e)
      : pos(p), end(e) {}

    void next() override {
      assert(pos != end);
      ++pos;
    }

    Fact::Ref get(Demand) override {
      return pos != end ? pos->ref() : Fact::Ref::invalid();
    }

    std::optional<Id> lower_bound() override { return std::nullopt; }
    std::optional<Id> upper_bound() override { return std::nullopt; }
  };

  return std::make_unique<Iterator>(
    lower_bound(from),
    upto ? lower_bound(upto) : end());
}

std::unique_ptr<FactIterator> FactSet::enumerateBack(Id from, Id downto) {
  struct BackIterator final : FactIterator {
    using iter_t = FactSet::const_iterator;
    iter_t pos;
    const iter_t end;

    BackIterator(iter_t p, iter_t e)
      : pos(p), end(e) {}

    void next() override {
      assert(pos != end);
      --pos;
    }

    Fact::Ref get(Demand) override {
      if (pos != end) {
        auto i = pos;
        --i;
        return i->ref();
      } else {
        return Fact::Ref::invalid();
      }
    }

    std::optional<Id> lower_bound() override { return std::nullopt; }
    std::optional<Id> upper_bound() override { return std::nullopt; }
  };

  return std::make_unique<BackIterator>(
    from ? lower_bound(from) : end(),
    lower_bound(downto));
}

std::unique_ptr<FactIterator> FactSet::seek(
    Pid type,
    folly::ByteRange start,
    size_t prefix_size) {
  struct SeekIterator : FactIterator {
    explicit SeekIterator(Index::map_t::iterator b, Index::map_t::iterator e)
      : current(b)
      , end(e)
    {}

    void next() override {
      assert(current != end);
      ++current;
    }

    Fact::Ref get(Demand) override {
      return current != end ? current->second->ref() : Fact::Ref::invalid();
    }

    std::optional<Id> lower_bound() override { return std::nullopt; }
    std::optional<Id> upper_bound() override { return std::nullopt; }

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
          for (const Fact *fact : *p) {
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
      next.empty()
        ? map.end()
        : map.lower_bound(binary::byteRange(next)));
  } else {
    return std::make_unique<EmptyIterator>();
  }
}

std::unique_ptr<FactIterator> FactSet::seekWithinSection(
    Pid type,
    folly::ByteRange start,
    size_t prefix_size,
    Id from,
    Id to ) {
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
  auto fact = Fact::create({next_id, type, clause});
  auto& key_map = keys[type];
  const auto r = key_map.insert(fact.get());
  if (r.second) {
    fact_memory += fact->size();
    facts.push_back(std::move(fact));
    return next_id;
  } else {
    return
      fact->value() == (*r.first)->value() ? (*r.first)->id() : Id::invalid();
  }
}

thrift::Batch FactSet::serialize() const {
  binary::Output output;
  for (auto& fact : *this) {
    fact.serialize(output);
  }

  thrift::Batch batch;
  batch.firstId() = startingId().toThrift();
  batch.count() = size();
  batch.facts() = output.moveToFbString();

  return batch;
}

///
// Serialize facts in the order given by the input range. Preconditions:
//
// * The order must mention only fact IDs in this set.
// * The facts in the set cannot refer to each other (because then we
//   would need to substitute in addition to reordering)
//
// The ordering can omit facts or mention facts multiple times,
// although I'm not sure why you would want to do that.
//
thrift::Batch
FactSet::serializeReorder(folly::Range<const uint64_t *> order) const {
  binary::Output output;
  for (auto i : order) {
    assert(i >= startingId().toWord() &&
           i - startingId().toWord() < facts.size());
    facts[i - startingId().toWord()]->serialize(output);
  }

  thrift::Batch batch;
  batch.firstId() = startingId().toThrift();
  batch.count() = size();
  batch.facts() = output.moveToFbString();

  return batch;
}

namespace {

template<typename Context>
std::pair<binary::Output, size_t> substituteFact(
    const Inventory& inventory,
    const Predicate::Rename<Context>& substitute,
    const Fact &fact) {
  auto predicate = inventory.lookupPredicate(fact.type());
  CHECK_NOTNULL(predicate);
  binary::Output clause;
  uint64_t key_size;
  predicate->substitute(substitute, fact.clause(), clause, key_size);
  return {std::move(clause), key_size};
}

}

FactSet FactSet::rebase(
    const Inventory& inventory,
    const Substitution& subst,
    Store& global) const {
  const auto new_start = subst.firstFreeId();
  const auto offset = distance(subst.finish(), new_start);
  const auto substitute = syscall([&subst, offset](Id id, Pid) {
    return id < subst.finish() ? subst.subst(id) : id + offset;
  });

  const auto split = lower_bound(subst.finish());

  for (auto& fact : folly::range(begin(), split)) {
    auto r = substituteFact(inventory, substitute, fact);
    global.insert({
      subst.subst(fact.id()),
      fact.type(),
      Fact::Clause::from(r.first.bytes(), r.second)
    });
  }

  FactSet local(new_start);
  auto expected = new_start;
  for (auto& fact : folly::range(split, end())) {
    auto r = substituteFact(inventory, substitute, fact);
    const auto id =
      local.define(fact.type(), Fact::Clause::from(r.first.bytes(), r.second));
    CHECK(id == expected);
    ++expected;
  }

  return local;
}

void FactSet::append(FactSet other) {
  assert(appendable(other));

  facts.insert(
    facts.end(),
    std::make_move_iterator(other.facts.begin()),
    std::make_move_iterator(other.facts.end()));

  keys.merge(std::move(other.keys), [](auto& left, const auto& right) {
    left.insert(right.begin(), right.end());
  });

  fact_memory += other.fact_memory;
}

bool FactSet::appendable(const FactSet& other) const {
  if (empty() || other.empty()) {
    return true;
  }

  if (firstFreeId() != other.startingId()) {
    return false;
  }

  for (const auto& k : other.keys) {
    if (const auto *p = keys.lookup(k.first)) {
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


}
}
}
