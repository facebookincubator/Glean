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

/// Entry mapping a directory to a list of ACL group UsetIds.
/// All groups at the same directory level are ORed together.
struct ACLConfigEntry {
  std::string directory;
  std::vector<UsetId> groupUsetIds;
};

/// Per-file ACL assignment: a UnitId (representing a single file path)
//  and its matching ACL levels.
/// Each inner vector represents groups at one directory level (ORed).
/// The outer vector represents levels that are ANDed together.
struct UnitACLAssignment {
  UnitId unitId;
  std::vector<std::vector<UsetId>> levels; // outer=AND, inner=OR
};

/// Augment computed ownership with ACL constraints.
///
/// For each UnitACLAssignment `a`:
/// 1. Transform a.levels vec of vec into a proper boolean expression Uset using
/// Set32 (outer vec -> AND, inner vec -> OR)
/// 2. for each (fact, uset) in `ownership`, for each unit in uset, get the CNF
/// for that unit from output of 1.,
//  and AND those and the uset
///
/// @param ownership  The computed ownership (modified in-place)
/// @param assignments  Per-unit ACL assignments (UnitId → levels of group IDs)
void augmentOwnershipWithACL(
    ComputedOwnership& ownership,
    const std::vector<UnitACLAssignment>& assignments);

} // namespace rts
} // namespace glean
} // namespace facebook
