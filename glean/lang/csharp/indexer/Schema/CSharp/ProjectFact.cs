/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

namespace Glean.Indexer.Schema.CSharp;

public record struct ProjectFactKey
    ( ProjectSource Source
    );

public record ProjectFact(ProjectFactKey Key) : FactWithKey<ProjectFactKey>(Predicate.Project, Key)
{
    public static ProjectFact MSBuild(string absoluteProjectPath)
    {
        var msbuildProjectSourceFact = new MSBuildProjectSourceFact(Hg.GetRepoRootRelativePath(absoluteProjectPath));
        var projectSource = ProjectSource.MSBuild(msbuildProjectSourceFact);
        return new ProjectFact(new ProjectFactKey(projectSource));
    }
}
