/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

using System.IO;
using Glean.Discovery;
using Glean.Indexer.Schema.Src;

namespace Glean.Indexer.Schema.CSharp;

public record struct ProjectFactKey
    ( ProjectSource Source
    );

public record ProjectFact(ProjectFactKey Key) : FactWithKey<ProjectFactKey>(Predicate.Project, Key)
{
    public static ProjectFact MSBuild(MaterializedWorkItem.MSBuildProject msbuildProjectWorkItem)
    {
        var msbuildProjectSourceFact = new MSBuildProjectSourceFact(Hg.GetRepoRootRelativePath(msbuildProjectWorkItem.ProjectPath));
        var projectSource = ProjectSource.MSBuild(msbuildProjectSourceFact);
        return new ProjectFact(new ProjectFactKey(projectSource));
    }

    public static ProjectFact Unity(MaterializedWorkItem.UnityPackage unityPackageWorkItem)
    {
        var projectPath = unityPackageWorkItem.GeneratedProjectPath;

        var unityPackageFactKey = new UnityPackageFactKey
            ( Type: unityPackageWorkItem.PackageType
            , Name: unityPackageWorkItem.PackageName
            );
        var unityProjectSourceFactKey = new UnityProjectSourceFactKey
            ( ProjectBasename: Path.GetFileName(projectPath)
            , UnityPackage: new UnityPackageFact(unityPackageFactKey)
            , AssemblyType: unityPackageWorkItem.AssemblyDefinitionType
            , ProjectTemplate: new FileFact(Hg.GetRepoRootRelativePath(unityPackageWorkItem.TemplatePath))
            );
        var unityProjectSourceFact = new UnityProjectSourceFact(unityProjectSourceFactKey);
        var projectSource = ProjectSource.Unity(unityProjectSourceFact);
        return new ProjectFact(new ProjectFactKey(projectSource));
    }
}
