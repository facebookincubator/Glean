/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <gtest/gtest.h>
#include <rapidcheck.h>
#include <rapidcheck/gtest.h>
#include <vector>

#include "glean/rts/substitution.h"
#include "glean/rts/tests/arbitrary.h"

using namespace facebook::glean::rts;

namespace rc {

template<>
struct Arbitrary<Substitution> {
  static Gen<Substitution> arbitrary() {
    return gen::construct<Substitution>(
      gen::arbitrary<Id>(),
      gen::arbitrary<std::vector<Id>>()
    );
  }
};

}

namespace facebook {
namespace glean {
namespace rts{

template <class T>
struct SubstitutionTest : testing::Test {};

RC_GTEST_PROP(
    SubstitutionTest,
    startFinish,
    (Id start, const std::vector<Id>& ids)) {
  Substitution subst(start,ids);
  RC_ASSERT(subst.start() == start);
  RC_ASSERT(subst.finish() == start + ids.size());
}

RC_GTEST_PROP(
    SubstitutionTest,
    substitute,
    (Id start, const std::vector<Id>& ids)) {
  Substitution subst(start,ids);

  for (size_t i = 0; i < ids.size(); ++i) {
    RC_ASSERT(subst.subst(start+i) == ids[i]);
  }

  auto identical = [&](Id id) { return subst.subst(id) == id; };

  RC_ASSERT(identical(Id::invalid()));
  RC_ASSERT(identical(Id::lowest()-1));
  RC_ASSERT(identical(subst.start()-1));
  RC_ASSERT(identical(subst.finish()));
  RC_ASSERT(identical(subst.finish()+1));
}

RC_GTEST_PROP(
    SubstitutionTest,
    firstFreeId,
    (Id start, const std::vector<Id>& ids)) {
  Substitution subst(start,ids);
  RC_ASSERT(subst.subst(subst.finish()) == subst.finish());
  const auto k = subst.firstFreeId();
  RC_ASSERT(k >= subst.finish());
  for (auto i : ids) {
    RC_ASSERT(k > i);
  }
}

RC_GTEST_PROP(
    SubstitutionTest,
    setAt,
    (Id start, const std::vector<Id>& ids)) {
  MutableSubstitution subst(start,ids);
  for (size_t i = 0; i < ids.size(); ++i) {
    subst.setAt(i, ids[ids.size() - i - 1]);
  }
  const std::vector<Id> rev(ids.rbegin(), ids.rend());
  RC_ASSERT(subst.freeze() == Substitution(start, rev));
}

RC_GTEST_PROP(SubstitutionTest, compose1, (const Substitution& subst)) {
  RC_ASSERT(
    Substitution::compose(subst, Substitution(subst.start(), {})) == subst);
}

RC_GTEST_PROP(SubstitutionTest, compose2, (const Substitution& subst)) {
  Substitution empty(subst.start(), {});
  RC_ASSERT(Substitution::compose(empty, subst) == empty);
}

RC_GTEST_PROP(
    SubstitutionTest,
    compose3,
    (const Substitution& s, const Substitution& t, const Substitution& u)) {
  RC_ASSERT(
    Substitution::compose(s,Substitution::compose(t,u))
      == Substitution::compose(Substitution::compose(s,t),u));
}

TEST(SubstitutionTest, intervals) {
  Id p = Id::lowest();

  MutableSubstitution msubst(p + 10, 1);
  msubst.setAt(0, p + 11); // { p+10 -> p }
  auto subst = msubst.freeze();

  std::vector<Id> intervals = {p, p + 1, p + 8, p + 10};

  EXPECT_EQ(
      subst.substIntervals(intervals),
      (std::vector<Id>{p, p + 1, p + 8, p + 9, p + 11, p + 11}));

  std::vector<Id> intervals2 = {p, p + 1, p + 8, p + 10, p + 11, p + 12};
  // 0-1 8-10 11-12
  //   rebase maps 10 -> 11 and 11-12 becomes 12-13
  // 0-1 8-9 11-13

  EXPECT_EQ(
      subst.rebaseIntervals(intervals2),
      (std::vector<Id>{p, p + 1, p + 8, p + 9, p + 11, p + 13})
  );
}

}
}
}
