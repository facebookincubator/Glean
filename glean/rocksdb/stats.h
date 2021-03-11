#pragma once

#include "glean/rts/densemap.h"
#include "glean/rts/id.h"
#include "glean/rts/stats.h"

namespace facebook {
namespace glean {
namespace rocks {

using PredicateStats = rts::DenseMap<rts::Pid, rts::MemoryStats>;

/// Atomic access to predicate stats. This is completely encapsulated here
/// mostly because including Hazptr.h needs RTTI, but we have to compile the
/// rest of rocksdb.cpp with -fno-rtti because some pre-compiled RocksDB
/// packages are compiled with -fno-rtti and otherwise we get linker errors.
class AtomicPredicateStats {
public:
  AtomicPredicateStats();
  ~AtomicPredicateStats();

  void set(PredicateStats stats);
  PredicateStats get() const;
  size_t count(rts::Pid pid) const;

  const PredicateStats& unprotected() const;

private:
  class Impl;
  std::unique_ptr<Impl> impl;
};

}
}
}
