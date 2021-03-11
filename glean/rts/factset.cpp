#include "glean/rts/factset.h"

namespace facebook {
namespace glean {
namespace rts {

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

    Index::map_t::const_iterator current;
    const Index::map_t::const_iterator end;
  };

  assert(prefix_size <= start.size());

  if (const auto p = keys.lookup(type)) {
    auto& entry = index[type];
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

Id FactSet::define(Pid type, Fact::Clause clause, Id) {
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
  batch.firstId_ref() = startingId().toThrift();
  batch.count_ref() = size();
  batch.facts_ref() = output.moveToFbString();

  return batch;
}

namespace {

std::pair<binary::Output, size_t> substituteFact(
    const Inventory& inventory,
    const Substituter& substituter,
    const Fact &fact) {
  auto predicate = inventory.lookupPredicate(fact.type());
  CHECK_NOTNULL(predicate);
  binary::Output clause;
  uint64_t key_size;
  predicate->substitute(substituter, fact.clause(), clause, key_size);
  return {std::move(clause), key_size};
}

}

FactSet FactSet::rebase(
    const Inventory& inventory,
    const Substitution& subst,
    Store& global) const {
  const auto new_start = subst.firstFreeId();
  Substituter substituter(&subst, distance(subst.finish(), new_start));

  const auto split = lower_bound(subst.finish());

  for (auto& fact : folly::range(begin(), split)) {
    auto r = substituteFact(inventory, substituter, fact);
    global.insert({
      subst.subst(fact.id()),
      fact.type(),
      Fact::Clause::from(r.first.bytes(), r.second)
    });
  }

  FactSet local(new_start);
  auto expected = new_start;
  for (auto& fact : folly::range(split, end())) {
    auto r = substituteFact(inventory, substituter, fact);
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

struct FactSet::Index::Impl {
  Impl() {}
  Impl(Impl&&) = default;
  Impl& operator=(Impl&&) = default;
  Impl(const Impl&) = delete;
  Impl& operator=(const Impl&) = delete;

  entry_t& operator[](Pid pid) {
    // Try getting an existing entry with a reader lock. If there is no entry,
    // obtain a writer lock and create one if it still doesn't exist.
    auto p = index.withRLock([pid](auto& locked) {
      const auto q = locked.lookup(pid);
      return q ? q->get() : nullptr;
    });
    if (!p) {
      p = index.withWLock([pid](auto& locked) {
        auto& r = locked[pid];
        if (r.get() == nullptr) {
          r.reset(new entry_t);
        }
        return r.get();
      });
    }
    return *p;
  }

  folly::Synchronized<DenseMap<Pid, std::unique_ptr<entry_t>>> index;
};

FactSet::Index::~Index() {
  delete impl.load(std::memory_order_relaxed);
}

FactSet::Index::Index(FactSet::Index&& other) noexcept
  : impl(other.impl.exchange(nullptr, std::memory_order_acq_rel))
{}

FactSet::Index& FactSet::Index::operator=(FactSet::Index&& other) {
  auto ptr = other.impl.exchange(nullptr, std::memory_order_acq_rel);
  ptr = impl.exchange(ptr, std::memory_order_release);
  delete ptr;
  return *this;
}

FactSet::Index::entry_t& FactSet::Index::operator[](Pid pid) {
  auto p = impl.load(std::memory_order_relaxed);
  if (p == nullptr) {
    auto k = std::make_unique<Impl>();
    if (impl.compare_exchange_strong(p, k.get(), std::memory_order_acq_rel)) {
      p = k.release();
    }
  }
  return (*p)[pid];
}

bool FactSet::sanityCheck() const {
  // TODO: implement
  return true;
}


}
}
}
