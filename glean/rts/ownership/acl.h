/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include "glean/rts/ownership.h"
#include "glean/rts/ownership/uset.h"

#include <string>
#include <vector>

namespace facebook {
namespace glean {
namespace rts {

/// Entry mapping a directory prefix to a list of ACL group UsetIds.
/// All groups at the same prefix level are ORed together.
struct ACLConfigEntry {
  std::string prefix;
  std::vector<UsetId> groupUsetIds;
};

/// Per-unit ACL assignment: a UnitId and its matching ACL levels.
/// Each inner vector represents groups at one directory prefix level (ORed).
/// The outer vector represents levels that are ANDed together.
struct UnitACLAssignment {
  UnitId unitId;
  std::vector<std::vector<UsetId>> levels; // outer=AND, inner=OR
};

/// Augment computed ownership with ACL constraints.
///
/// For each unit that has ACL assignments, this function:
/// 1. Creates OR(groups) for each directory level
/// 2. ANDs all level OR-sets to get a CNF UsetId per unit
/// 3. Walks facts_ and ANDs each fact's existing owner with the unit's CNF
///
/// @param ownership  The computed ownership (modified in-place)
/// @param assignments  Per-unit ACL assignments (UnitId → levels of group IDs)
void augmentOwnershipWithACL(
    ComputedOwnership& ownership,
    const std::vector<UnitACLAssignment>& assignments);

} // namespace rts
} // namespace glean
} // namespace facebook
