/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "glean/rts/substitution.h"

namespace facebook {
namespace glean {
namespace rts {

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
  for (auto p : is) {
    results.push_back(p.lower());
    results.push_back(p.upper());
  }
  return results;
}

}

void Substitution::rebaseInterval(boost::icl::interval_set<Id>& is, size_t offset, Id start, Id end) const {
  if (end < base) {
    is.add({start, end});
  } else {
    if (start >= finish()) {
      is.add({start + offset, end + offset});
    }
    for (Id id = start; id <= end; ++id) {
      if (id >= finish()) {
        is.add(id + offset);
      } else {
        is.add(subst(id));
      }
    }
  };
}

std::vector<Id> Substitution::substIntervals(
    const std::vector<Id>& intervals) const {
  return transformIntervals(
      intervals, [&](boost::icl::interval_set<Id>& is, Id start, Id end) {
        if (end < base || start >= finish()) {
          is.add({start, end});
        } else {
          for (Id id = start; id <= end; ++id) {
            is.add(subst(id));
          }
        }
      });
}

std::vector<Id> Substitution::rebaseIntervals(
    const std::vector<Id>& intervals) const {
  const auto new_start = firstFreeId();
  const auto offset = distance(finish(), new_start);
  return transformIntervals(
      intervals,
      [&, offset](boost::icl::interval_set<Id>& is, Id start, Id end) {
        rebaseInterval(is, offset, start, end);
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

boost::icl::interval_set<Id> Substitution::rebaseIntervals(const boost::icl::interval_set<Id>& intervals) const {
  boost::icl::interval_set<Id> is;
  const auto offset = distance(finish(), firstFreeId());
  for (auto ival : intervals) {
    rebaseInterval(is, offset, ival.lower(), ival.upper());
  }
  return is;
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
