/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include "glean/rts/ownership/uset.h"

#include <vector>

namespace facebook::glean::rts {

/// Builds canonical non-empty expressions whose IDs can be stored as owners.
/// Combining flattens matching operations and removes duplicates. A single
/// expression retains its operation; a single unit is stored as OR(unit).
/// Promoted expressions stay alive until the supplied Usets store is destroyed.
class OwnershipExpressionBuilder {
 public:
  explicit OwnershipExpressionBuilder(Usets& expressions)
      : expressions_(expressions) {}

  /// Return the OR of members; members must be non-empty.
  UsetId anyOf(const std::vector<UsetId>& members);

  /// Return the AND of members; members must be non-empty.
  UsetId allOf(const std::vector<UsetId>& members);

 private:
  UsetId make(SetOp op, const std::vector<UsetId>& members);

  Usets& expressions_;
};

} // namespace facebook::glean::rts
