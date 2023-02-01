/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include <vector>
#include <boost/icl/interval_set.hpp>

#include "glean/rts/id.h"

namespace facebook {
namespace glean {
namespace rts {

/**
 * Substitutions for blocks of consecutive Ids.
 *
 * A substitution implements a mapping Id -> Id for its range from
 * start() up to but not including finish(). Ids outside of this range are
 * mapped to themselves.
 *
 */
class Substitution {
public:
  Substitution(Id first, size_t size);

  Substitution(Id first, std::vector<Id> ids);

  Id subst(Id id) const {
    return id >= start() && id < finish()
      ? items[distance(start(),id)]
      : id;
  }

  // Apply the Substitution to a set of fact ID ranges.
  //
  // Input/output: a list of ranges, [x1,x2, y1,y2, ...]
  //
  // The input doesn't need to be sorted or non-overlapping, but the
  // output will be non-overlapping and sorted in ascending order.
  std::vector<Id> substIntervals(const std::vector<Id>& intervals) const;

  boost::icl::interval_set<Id> substIntervals(
      const boost::icl::interval_set<Id>& intervals) const;

  Id start() const {
    return base;
  }

  Id finish() const {
    return base + items.size();
  }

  void set(Id pos, Id id) {
    CHECK(pos >= start() && pos < finish());
    items[distance(start(),pos)] = id;
  }

  void setAt(size_t index, Id id) {
    items[index] = id;
  }

  Id firstFreeId() const;

  static Substitution compose(
    const Substitution& first,
    const Substitution& second);

  bool operator==(const Substitution& other) const {
    return base == other.base && items == other.items;
  }

  bool operator!=(const Substitution& other) const {
    return !(*this == other);
  }

  void with(const std::function<void(Id base, const std::vector<Id>& items)>& fun) const {
    fun(base, items);
  }

  bool sanityCheck(bool incomplete) const;

private:
  Id base;
  std::vector<Id> items;
};

}
}
}
