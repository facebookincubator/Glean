// Copyright (c) Facebook, Inc. and its affiliates.

#include <folly/experimental/AutoTimer.h>

#include "glean/rts/ownership/derived.h"

namespace facebook {
namespace glean {
namespace rts {

void DefineOwnership::derivedFrom(Id id, const std::set<UsetId>& deps) {
  if (deps.size() == 0) {
    LOG(ERROR) << "DefineOwnership::derivedFrom: empty deps";
    return;
  }

  SetU32 set = SetU32::from(deps);

  UsetId usetid;
  if (set.size() == 1) {
    usetid = *deps.begin();
  } else {
    auto uset = std::make_unique<Uset>(set, And, 0);
    size_t size = set.size();
    usetid = ownership_->lookupSet(uset.get());
    if (usetid == INVALID_USET) {
      auto q = uset.get();
      auto p = usets_.add(std::move(uset));
      if (p == q) {
        usets_.promote(p);
        usetid = p->id;
        newSets_.push_back(p);
        VLOG(2) << "new set: " << usetid << ", size = " << size;
      } else {
        usetid = p->id;
        VLOG(2) << "existing set in batch: " << usetid;
      }
    } else {
      VLOG(2) << "existing set in DB: " << usetid;
    }
  }
  ids_.push_back(id);
  owners_.push_back(usetid);
}

void DefineOwnership::subst(const Substitution& subst) {
  for (auto& id : ids_) {
    id = subst.subst(id);
  }
}

}
}
}
