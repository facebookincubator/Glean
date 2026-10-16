/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "glean/rts/ownership/expression.h"

#include <set>

namespace facebook::glean::rts {

UsetId OwnershipExpressionBuilder::anyOf(const std::vector<UsetId>& members) {
  return make(Or, members);
}

UsetId OwnershipExpressionBuilder::allOf(const std::vector<UsetId>& members) {
  return make(And, members);
}

UsetId OwnershipExpressionBuilder::make(
    SetOp op,
    const std::vector<UsetId>& members) {
  CHECK(!members.empty());

  std::set<UsetId> normalized;
  for (const auto member : members) {
    if (auto* expression = expressions_.lookupById(member);
        expression != nullptr && expression->exp.op == op) {
      expression->exp.set.foreach(
          [&](UsetId nested) { normalized.insert(nested); });
    } else {
      normalized.insert(member);
    }
  }

  if (normalized.size() == 1) {
    const auto member = *normalized.begin();
    if (expressions_.lookupById(member) != nullptr) {
      return member;
    }
    op = Or;
  }

  auto expression =
      std::make_unique<Uset>(SetU32::from(normalized), op, /* refs */ 1);
  auto* canonical = expressions_.add(std::move(expression));
  expressions_.promote(canonical);
  return canonical->id;
}

} // namespace facebook::glean::rts
