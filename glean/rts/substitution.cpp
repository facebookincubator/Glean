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
   const auto i = std::max_element(items.begin(), items.end());
   return i != items.end() ? std::max(*i+1, finish()) : finish();
 }

std::vector<Id> Substitution::substIntervals(const std::vector<Id>& intervals) const {
  CHECK_EQ(intervals.size() % 2, 0);
  boost::icl::interval_set<Id> is;
  auto i = intervals.begin();
  const auto e = intervals.end();
  while (i != e) {
    const auto start = i[0];
    const auto finish = i[1];
    CHECK(start <= finish);
    if (finish < base) {
      is.add({start,finish});
    } else {
      for (Id id = start; id <= finish; ++id) {
        is.add(subst(id));
      }
    }
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

thrift::Subst Substitution::serialize() const {
  std::vector<thrift::Id> ids;
  ids.reserve(items.size());
  std::transform(
    items.begin(),
    items.end(),
    std::back_inserter(ids),
    [](auto x) { return x.toThrift(); }
  );
  thrift::Subst s;
  s.firstId_ref() = base.toThrift();
  s.ids_ref() = std::move(ids);
  return s;
}

Substitution Substitution::deserialize(const thrift::Subst& subst) {
  Substitution s(Id::fromThrift(subst.get_firstId()), subst.get_ids().size());
  std::transform(
    subst.get_ids().begin(),
    subst.get_ids().end(),
    s.items.begin(),
    Id::fromThrift
  );
  return s;
}

bool Substitution::sanityCheck(bool incomplete) const {
  if (base == Id::invalid()) {
    if (!items.empty()) {
      LOG(ERROR) << "substitution with base 0 and size " << items.size();
      return false;
    }
  } else {
    if (base < Id::lowest()) {
      LOG(ERROR) << "substitution with base " << base.toThrift();
      return false;
    }
    for (auto id : items) {
      if (id < Id::lowest() && !(incomplete && !id)) {
        LOG(ERROR) << "substitution with invalid id " << id.toThrift();
        return false;
      }
    }
  }
  return true;
}

}
}
}
