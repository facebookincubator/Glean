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

#include <glean/rts/ownership.h>
#include "glean/rts/substitution.h"
#include "glean/rts/tests/arbitrary.h"

using namespace facebook::glean::rts;

struct Intervals {
  Substitution subst;
  std::vector<Id> intervals;
};

namespace rc {

template <>
struct Arbitrary<Substitution> {
  static Gen<Substitution> arbitrary() {
    return gen::construct<Substitution>(
        gen::arbitrary<Id>(), gen::arbitrary<std::vector<Id>>());
  }
};

template <>
struct Arbitrary<Intervals> {
  static Gen<Intervals> arbitrary() {
    return gen::mapcat(
        gen::arbitrary<Substitution>(),
        [](const Substitution& subst) -> Gen<Intervals> {
          auto genId = [](Id hi) {
            return gen::map(
                gen::inRange(Id::lowest().toWord(), hi.toWord()),
                [](uint64_t w) { return Id::fromWord(w); });
          };
          if (subst.finish() == Id::lowest()) {
            return gen::construct<Intervals>(
                gen::just(subst), gen::just<std::vector<Id>>({}));
          }
          return gen::construct<Intervals>(
              gen::just(subst),
              gen::map(
                  gen::container<std::vector<std::pair<Id, Id>>>(
                      gen::pair(genId(subst.finish()), genId(subst.finish()))),
                  [](std::vector<std::pair<Id, Id>> v) -> std::vector<Id> {
                    std::vector<Id> out;
                    for (const auto& p : v) {
                      out.push_back(std::min(p.first, p.second));
                      out.push_back(std::max(p.first, p.second));
                    };
                    return out;
                  }));
        });
  }
};

} // namespace rc

namespace facebook {
namespace glean {
namespace rts {

template <class T>
struct SubstitutionTest : testing::Test {};

std::ostream& operator<<(std::ostream& out, Substitution subst) {
  out << "{";
  for (Id i = subst.start(); i != subst.finish(); ++i) {
    out << i << ": " << subst.subst(i);
    if (i != subst.finish() - 1) {
      out << ", ";
    }
  }
  out << "}";
  return out;
}

RC_GTEST_PROP(
    SubstitutionTest,
    startFinish,
    (Id start, const std::vector<Id>& ids)) {
  Substitution subst(start, ids);
  RC_ASSERT(subst.start() == start);
  RC_ASSERT(subst.finish() == start + ids.size());
}

RC_GTEST_PROP(
    SubstitutionTest,
    substitute,
    (Id start, const std::vector<Id>& ids)) {
  Substitution subst(start, ids);

  for (size_t i = 0; i < ids.size(); ++i) {
    RC_ASSERT(subst.subst(start + i) == ids[i]);
  }

  auto identical = [&](Id id) { return subst.subst(id) == id; };

  RC_ASSERT(identical(Id::invalid()));
  RC_ASSERT(identical(Id::lowest() - 1));
  RC_ASSERT(identical(subst.start() - 1));
  RC_ASSERT(identical(subst.finish()));
  RC_ASSERT(identical(subst.finish() + 1));
}

RC_GTEST_PROP(
    SubstitutionTest,
    firstFreeId,
    (Id start, const std::vector<Id>& ids)) {
  Substitution subst(start, ids);
  RC_ASSERT(subst.subst(subst.finish()) == subst.finish());
  const auto k = subst.firstFreeId();
  RC_ASSERT(k >= subst.finish());
  for (auto i : ids) {
    RC_ASSERT(k > i);
  }
}

RC_GTEST_PROP(SubstitutionTest, setAt, (Id start, const std::vector<Id>& ids)) {
  MutableSubstitution subst(start, ids);
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

RC_GTEST_PROP(SubstitutionTest, substIntervals, (const Intervals& intervals)) {
  auto out = intervals.subst.substIntervals(intervals.intervals);

  using namespace boost::icl;
  boost::icl::interval_set<Id, std::less, closed_interval<Id>> is;
  for (auto i = out.begin(); i != out.end(); i += 2) {
    is.insert({i[0], i[1]});
  }

  for (auto i = intervals.intervals.begin(); i != intervals.intervals.end();
       i += 2) {
    RC_ASSERT(contains(is, intervals.subst.subst(i[0])));
    RC_ASSERT(contains(is, intervals.subst.subst(i[1])));
  }
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
}

} // namespace rts
} // namespace glean
} // namespace facebook
