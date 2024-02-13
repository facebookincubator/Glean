/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

using Glean.Discovery;
using Glean.Indexer.Schema.Src;
using System.IO;
using Newtonsoft.Json;

namespace Glean.Indexer.Schema.CSharp;

public record ProjectSource(UnityProjectSourceFact? UnityProjectSource = null, MSBuildProjectSourceFact? MsbuildProjectSource = null)
{
    public static ProjectSource MSBuild(MSBuildProjectSourceFact msbuildProjectSource)
        => new (null, msbuildProjectSource);

    public static ProjectSource MSBuild(MaterializedWorkItem.MSBuildProject msbuildProjectWorkItem)
    {
        var msbuildProjectSourceFact = new MSBuildProjectSourceFact(Hg.GetRepoRootRelativePath(msbuildProjectWorkItem.ProjectPath));
        return MSBuild(msbuildProjectSourceFact);
    }

    public static ProjectSource Unity(UnityProjectSourceFact unityProjectSource)
        => new (unityProjectSource, null);

    public static ProjectSource Unity(MaterializedWorkItem.UnityPackage unityPackageWorkItem)
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
        return ProjectSource.Unity(unityProjectSourceFact);
    }
}
