/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

using Glean.Indexer.Schema.Src;

namespace Glean.Indexer.Schema.CSharp;

public record struct ProjectToSourceFileFactKey
    ( ProjectFact Project
    , FileFact Src
    );

public record ProjectToSourceFileFact(ProjectToSourceFileFactKey Key) : FactWithKey<ProjectToSourceFileFactKey>(Predicate.ProjectToSourceFile, Key);
