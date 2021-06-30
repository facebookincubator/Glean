// Copyright (c) Facebook, Inc. and its affiliates.

#include "glean/rocksdb/stats.h"

#include <folly/synchronization/Hazptr.h>

namespace facebook {
namespace glean {
namespace rocks {

struct AtomicPredicateStats::Impl {
  // This needs to be protected by a hazard poiter for access. We support reads
  // concurrent to a write but not concurrent writes and a write replaces the
  // entire object so a hazptr is sufficient.
  struct Holder : folly::hazptr_obj_base<Holder> {
    PredicateStats stats;

    explicit Holder(PredicateStats s) : stats(std::move(s)) {}
  };

  std::atomic<Holder *> holder = new Holder({});

  // We have a separate cohort for each DB - the stats objects we create
  // are local to the DB so they should be destroyed when the DB object is
  // destroyed at the latest.
  folly::hazptr_obj_cohort<> cohort;
};

AtomicPredicateStats::AtomicPredicateStats()
  : impl(std::make_unique<Impl>())
{}

AtomicPredicateStats::~AtomicPredicateStats()
{
  delete impl->holder.load();
}

void AtomicPredicateStats::set(PredicateStats stats) {
  auto p = new Impl::Holder(std::move(stats));
  p->set_cohort_tag(&impl->cohort);
  p = impl->holder.exchange(p);
  if (p) {
    p->retire();
  }
}

const PredicateStats& AtomicPredicateStats::unprotected() const {
  return impl->holder.load()->stats;
}

PredicateStats AtomicPredicateStats::get() const {
  folly::hazptr_local<1> hptr;
  return hptr[0].protect(impl->holder)->stats;
}

size_t AtomicPredicateStats::count(rts::Pid pid) const {
  folly::hazptr_local<1> hptr;
  if (auto stat = hptr[0].protect(impl->holder)->stats.get(pid)) {
    return stat->count;
  } else {
    return {};
  }
}

}
}
}
