/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include "glean/rts/ownership.h"
#include "glean/rts/substitution.h"

namespace facebook {
namespace glean {
namespace rts {

///
// Container for holding ownership data for facts as they are derived
//
struct DefineOwnership {
  DefineOwnership(Ownership *ownership, Pid pid) :
      pid_(pid), ownership_(ownership), usets_(ownership->nextSetId()) {};

  // record that a fact was derived from some other facts
  void derivedFrom(Id id, const std::set<UsetId>& deps);

  UsetId getOwner(Id id) {
    return ownership_->getOwner(id);
  }

  // Apply a substitution to the Ids
  void subst(const Substitution& subst);

  const Pid pid_;
  Ownership *ownership_;
  std::vector<Id> ids_;
  std::vector<UsetId> owners_;
  Usets usets_;

  // new sets in order of creation, needed for rebasing the sets against
  // the DB later.
  std::vector<Uset*> newSets_;
};

struct DerivedFactOwnership {
  folly::Range<const Id*> ids;
  folly::Range<const UsetId*> owners;
};

struct DerivedFactOwnershipIterator {
  virtual ~DerivedFactOwnershipIterator() {}

  // Note: the result is only valid until the next call to get()
  virtual folly::Optional<DerivedFactOwnership> get() = 0;
};

///
// Compute ownership data for derived facts
//
std::unique_ptr<ComputedOwnership> computeDerivedOwnership(
  Ownership& ownership,
  DerivedFactOwnershipIterator *iter);

}
}
}
