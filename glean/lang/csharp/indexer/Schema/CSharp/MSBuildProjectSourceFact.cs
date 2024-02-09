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

public record struct MSBuildProjectSourceFactKey
    ( FileFact Src
    );

public record MSBuildProjectSourceFact(MSBuildProjectSourceFactKey Key) : FactWithKey<MSBuildProjectSourceFactKey>(Predicate.MSBuildProjectSource, Key)
{
    public MSBuildProjectSourceFact(string repoRootRelativeProjectPath) : this(new MSBuildProjectSourceFactKey(new FileFact(repoRootRelativeProjectPath))) { }
}
