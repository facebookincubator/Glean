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

// Builds canonical non-empty expressions whose IDs can be stored as owners.
// Promoted expressions stay alive until the supplied Usets store is destroyed.
class OwnershipExpressionBuilder {
 public:
  explicit OwnershipExpressionBuilder(Usets& expressions)
      : expressions_(expressions) {}

  // Combine non-empty members using Boolean OR or AND, returning an expression
  // ID. Flatten members with the same operation and remove duplicates. A single
  // expression is reused with its original operation: OR(x) == AND(x) == x.
  // A single unit is stored as OR(unit), shared by both operations.
  UsetId anyOf(const std::vector<UsetId>& members);
  UsetId allOf(const std::vector<UsetId>& members);

 private:
  UsetId make(SetOp op, const std::vector<UsetId>& members);

  Usets& expressions_;
};

} // namespace facebook::glean::rts
