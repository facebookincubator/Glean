/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

using Facebook.SocialVR.Packages;
using System.Text.Json;
using System.Text.Json.Serialization;

namespace Glean.Discovery;

[JsonConverter(typeof(MaterializedWorkItemConverter))]
public abstract record MaterializedWorkItem([property: JsonConverter(typeof(JsonStringEnumConverter))] WorkType Type)
{
    public record MSBuildProject(string ProjectPath)
        : MaterializedWorkItem(WorkType.MSBuildProject);

    public record MSBuildSolution(string SolutionPath, string[] ProjectPaths)
        : MaterializedWorkItem(WorkType.MSBuildSolution);

    public record UnityPackage
        ( string GeneratedProjectPath
        , [property: JsonConverter(typeof(JsonStringEnumConverter))] PackageType PackageType
        , string PackageName
        , string TemplatePath
        , [property: JsonConverter(typeof(JsonStringEnumConverter))] AssemblyDefinition.Type AssemblyDefinitionType
        ) : MaterializedWorkItem(WorkType.UnityPackage);

    public record BuckBuildTarget(string GeneratedProjectPath, string BuildTarget)
        : MaterializedWorkItem(WorkType.BuckBuildTarget);
}
