/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "glean/rts/substitution.h"
#include "glean/rts/error.h"

namespace facebook {
namespace glean {
namespace rts {

using namespace boost::icl;

Substitution::Substitution(Id first, size_t size)
  : base(first)
  , items(size, Id::invalid())
  {}

Substitution::Substitution(Id first, std::vector<Id> ids)
  : base(first)
  , items(std::move(ids))
  {}

Id Substitution::firstFreeId() const {
  if (!firstFreeId_) {
    const auto i = std::max_element(items.begin(), items.end());
    firstFreeId_ = i != items.end() ? std::max(*i+1, finish()) : finish();
  }
  return firstFreeId_;
}

namespace {

template <typename F>
std::vector<Id> transformIntervals(const std::vector<Id>& intervals, F&& add) {
  CHECK_EQ(intervals.size() % 2, 0);
  boost::icl::interval_set<Id> is;
  auto i = intervals.begin();
  const auto e = intervals.end();
  while (i != e) {
    const auto start = i[0];
    const auto end = i[1];
    CHECK(start <= end);
    add(is, start, end);
    i += 2;
  }
  std::vector<Id> results;
  results.reserve(is.iterative_size()*2);
  for (const auto& p : is) {
    results.push_back(p.lower());
    results.push_back(p.upper());
  }
  return results;
}

}

std::vector<Id> Substitution::substIntervals(
    const std::vector<Id>& intervals) const {
  return transformIntervals(
      intervals, [&](boost::icl::interval_set<Id>& is, Id start, Id end) {
        if (end >= finish()) {
          error("interval out of range: {}-{} base={} finish={}",
                start.toWord(), end.toWord(), base.toWord(), finish().toWord());
        }
        if (end < base) {
          is.add({start, end, interval_bounds::closed()});
        } else {
          if (start < base) {
            is.add({start, base-1, interval_bounds::closed()});
          }
          for (Id id = std::max(start,base); id <= end; ++id) {
            is.add(subst(id));
          }
        }
      });
}

boost::icl::interval_set<Id> Substitution::substIntervals(const boost::icl::interval_set<Id>& intervals) const {
  boost::icl::interval_set<Id> result;
  for (auto ival : intervals) {
    if (ival.upper() < base) {
      result.add(ival);
    } else {
      for (Id id = ival.lower(); id <= ival.upper(); ++id) {
        result.add(subst(id));
      }
    }
  }
  return result;
}

Substitution Substitution::compose(
    const Substitution& first,
    const Substitution& second) {
  std::vector<Id> ids;
  ids.reserve(first.items.size());
  for (auto id : first.items) {
    ids.push_back(id < second.finish() ? second.subst(id) : id);
  }
  return Substitution(first.base, std::move(ids));
}

bool Substitution::sanityCheck(bool incomplete) const {
  if (base == Id::invalid()) {
    if (!items.empty()) {
      LOG(ERROR) << "substitution with base 0 and size " << items.size();
      return false;
    }
  } else {
    if (base < Id::lowest()) {
      LOG(ERROR) << "substitution with base " << base.toWord();
      return false;
    }
    for (auto id : items) {
      if (id < Id::lowest() && !(incomplete && !id)) {
        LOG(ERROR) << "substitution with invalid id " << id.toWord();
        return false;
      }
    }
  }
  return true;
}

}
}
}
