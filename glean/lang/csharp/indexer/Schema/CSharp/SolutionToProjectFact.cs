/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

namespace Glean.Indexer.Schema.CSharp;

public record struct SolutionToProjectFactKey
    ( SolutionFact Solution
    , ProjectFact Project
    );

public record SolutionToProjectFact(SolutionToProjectFactKey Key) : FactWithKey<SolutionToProjectFactKey>(Predicate.SolutionToProject, Key);
