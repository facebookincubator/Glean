/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include "glean/rts/ownership.h"
#include "glean/rts/ownership/expression.h"
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

/// A directory prefix and the alternative accessibility group units it allows.
/// A path matches a rule when it equals the prefix or starts with prefix + "/".
/// All directory rules matching a path must be satisfied.
struct DirectoryACLRule {
  std::string directory;
  std::vector<UnitId> groups;
};

/// Per-file ACL assignment: a visibility source unit and its matching
/// directory rules. Each inner list contains the ACL group IDs
/// allowed by one rule (OR); the outer list contains all matching rules (AND).
struct UnitACLAssignment {
  UnitId unitId;
  std::vector<std::vector<UsetId>> levels;
};

/// Builds accessibility expressions from matched directory rules and combines
/// owners with Boolean AND/OR. Every ACL slice must select the public unit.
class ACLExpressionBuilder {
 public:
  /// Adds expressions to usets, which must outlive this builder. publicUnit is
  /// a leaf below usets.getFirstId(); its stored OR(publicUnit) expression is
  /// the owner of public facts.
  ACLExpressionBuilder(Usets& usets, UnitId publicUnit);

  /// Combines accessibility owners translated from a visibility expression's
  /// members, preserving its operator. Simplifies public and INVALID_USET
  /// owners and reuses equivalent expressions.
  UsetId ownerForPathExpression(SetOp op, const std::vector<UsetId>& owners);

  /// Builds the owner for a path from directory rules matched by the caller.
  /// Each inner list contains alternative accessibility group unit IDs (OR);
  /// all matching rules must be satisfied (AND). For example, {{a, b}, {c}}
  /// means (a OR b) AND c. An empty outer list is public; an empty inner list
  /// is inaccessible (INVALID_USET).
  UsetId ownerForPath(const std::vector<std::vector<UnitId>>& ruleGroups);

 private:
  UsetId combine(SetOp op, const std::vector<UsetId>& owners);

  OwnershipExpressionBuilder expressionBuilder_;
  UnitId publicUnit_;
  UsetId publicOwner_;
};

/// Augment computed ownership with ACL constraints.
///
/// For each assignment, combine alternative groups with OR and matching rules
/// with AND. Add these constraints to each fact whose owner contains the
/// assignment's visibility unit.
///
/// @param ownership  The computed ownership (modified in-place)
/// @param assignments  Visibility source units and their matched ACL rules
void augmentOwnershipWithACL(
    ComputedOwnership& ownership,
    const std::vector<UnitACLAssignment>& assignments);

} // namespace rts
} // namespace glean
} // namespace facebook
