// Copyright (c) Facebook, Inc. and its affiliates.

#include "glean/rts/ownership/uset.h"

namespace facebook {
namespace glean {
namespace rts {

SetExpr<Usets::MutableEliasFanoList> Uset::toEliasFano() {
  return { exp.op, exp.set.toEliasFano() };
}

std::vector<SetExpr<Usets::MutableEliasFanoList>> Usets::toEliasFano() {
  std::vector<SetExpr<Usets::MutableEliasFanoList>> sets(stats.promoted);
  for (auto uset : usets) {
    if (uset->promoted()) {
      VLOG(5) << "exporting: " << uset->id;
      sets[uset->id - firstId] = uset->toEliasFano();
    }
  }
  return sets;
}

}
}
}
