// Copyright (c) Facebook, Inc. and its affiliates.

#include "glean/rts/ownership/uset.h"

namespace facebook {
namespace glean {
namespace rts {

std::vector<Usets::EliasFanoList> Usets::toEliasFano() {
  std::vector<Usets::EliasFanoList> sets(stats.promoted);
  for (auto uset : usets) {
    if (uset->promoted()) {
      VLOG(5) << "exporting: " << uset->id;
      sets[uset->id] = uset->set.toEliasFano();
    }
  }
  return sets;
}

}
}
}
