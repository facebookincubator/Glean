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

#include <vector>

namespace facebook {
namespace glean {
namespace rts {

/// Build a CNF uset from ACL levels and return its promoted UsetId.
///
/// Each level is a list of group UsetIds (ORed together).
/// Levels are ANDed together to form the CNF expression.
///
/// @param ownership  The computed ownership (usets container is modified)
/// @param levels     ACL levels: outer=AND, inner=OR of group UsetIds
/// @return The promoted UsetId of the resulting CNF expression
UsetId makeACLCnf(
    ComputedOwnership& ownership,
    const std::vector<std::vector<UsetId>>& levels);

/// Augment computed ownership with ACL constraints.
///
/// Walks facts_ and ANDs each fact's existing owner with the matching
/// unit's prebuilt ACL CNF UsetId.
///
/// @param ownership  The computed ownership (modified in-place)
/// @param assignments  Per-unit ACL assignments (UnitId → CNF UsetId)
void augmentOwnershipWithACL(
    ComputedOwnership& ownership,
    const std::vector<std::pair<UnitId, UsetId>>& assignments);

} // namespace rts
} // namespace glean
} // namespace facebook
