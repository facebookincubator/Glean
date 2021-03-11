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
