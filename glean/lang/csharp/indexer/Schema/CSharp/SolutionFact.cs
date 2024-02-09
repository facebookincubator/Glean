/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

using Glean.Indexer.Schema;
using Glean.Indexer.Schema.Src;

namespace Glean.Indexer.Schema.CSharp;

public record struct SolutionFactKey
    ( FileFact Src
    );

public record SolutionFact(SolutionFactKey Key) : FactWithKey<SolutionFactKey>(Predicate.Solution, Key)
{
    public SolutionFact(string repoRootRelativeSolutionPath) : this(new SolutionFactKey(new FileFact(repoRootRelativeSolutionPath))) { }
}
