#pragma once

#include "glean/rts/ownership/setu32.h"

#include <folly/container/F14Set.h>
#include <folly/Hash.h>
#include <vector>

namespace facebook {
namespace glean {
namespace rts {

/**
 * A "unique" set stored by `Usets` below. This is a `SetU32` with a memoized
 * hash, a ref count and some administrative data used by the ownership
 * algorithms. It should probably be given a more sane interface.
 */
struct Uset {
  explicit Uset(SetU32 s, uint32_t r = 0) : set(std::move(s)), refs(r) {}

  void rehash() {
    hash = set.hash();
  }

  void use(int32_t n) {
    refs += n;
  }

  void *link() const {
    return ptr;
  }

  void link(void *p) {
    ptr = p;
  }

  /** The set */
  SetU32 set;

  /** The set's hash which must be memoized explicitly via `rehash`. */
  size_t hash;

  /** Reference count */
  uint32_t refs;

  /**
   * Has the set been promoted to the DB - i.e., is it the actual ownership
   * set of at least one fact. This will most likely store some kind of id
   * when we actually stored sets in the DB.
   */
  bool promoted = false;

  /**
   * Generic pointer used temporarily for a variety of things - it is much
   * faster than a F14FastMap<Uset *, T>.
   */
  void *ptr = nullptr;

  struct Eq {
    bool operator()(const Uset *x, const Uset *y) const {
      return x == y || x->set == y->set;
    }
  };

  struct Hash {
    using folly_is_avalanching = std::true_type;
    size_t operator()(const Uset *x) const {
      return x->hash;
    }
  };
};

/**
 * Container for `Usets` which guarantees to store each set exactly once.
 */
struct Usets {
  Usets() = default;

  Usets(Usets&& other) {
    std::swap(usets, other.usets);
    stats = other.stats;
  }

  ~Usets() {
    // We can't delete the Usets before destroying the `usets` map because
    // the latter might access hashes in its destructor.
    std::vector<Uset *> refs;
    refs.reserve(usets.size());
    refs.insert(refs.end(), usets.begin(), usets.end());
    usets.clear();
    for (auto ref : refs) {
      delete ref;
    }
  }

  template<typename F>
  void foreach(F&& f) {
    for (auto entry : usets) {
      f(entry);
    }
  }

  Uset *add(Uset *entry) {
    entry->rehash();
    const auto [p, added] = usets.insert(entry);
    if (added) {
      entry->set.shrink_to_fit();
      ++stats.adds;
      stats.bytes += entry->set.bytes();
      return entry;
    } else {
      ++stats.dups;
      use(*p, entry->refs);
      return *p;
    }
  }

  Uset *add(std::unique_ptr<Uset> entry) {
    auto p = add(entry.get());
    if (p == entry.get()) {
      entry.release();
    }
    return p;
  }

  Uset *add(SetU32 set, uint32_t refs) {
    return add(std::unique_ptr<Uset>(new Uset(std::move(set), refs)));
  }

  Uset *merge(Uset *left, Uset *right) {
    SetU32 set;
    auto res = SetU32::merge(set, left->set, right->set);
    if (res == &set) {
      return add(std::move(set), 1);
    } else {
      auto& r = res == &left->set ? left : right;
      use(r, 1);
      return r;
    }
  }

  void use(Uset *set, uint32_t refs = 1) {
    set->refs += refs;
  }

  void drop(Uset *uset) {
    assert(uset->refs != 0);
    --uset->refs;
    if (uset->refs == 0) {
      assert(!uset->promoted);
      usets.erase(uset);
      stats.bytes -= uset->set.bytes();
      delete uset;
    }
  }

  void promote(Uset * uset) {
    if (!uset->promoted) {
      uset->promoted = true;
      ++uset->refs;
      ++stats.promoted;
    }
  }

  size_t size() const {
    return usets.size();
  }

  struct Stats {
    size_t bytes = 0;
    size_t promoted = 0;

    size_t adds = 0;
    size_t dups = 0;
  };

  const Stats& statistics() const {
    return stats;
  }

private:
  folly::F14FastSet<Uset *, Uset::Hash, Uset::Eq> usets;
  Stats stats;
};

}
}
}
