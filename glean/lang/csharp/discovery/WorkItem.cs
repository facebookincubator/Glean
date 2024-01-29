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

[JsonConverter(typeof(WorkItemConverter))]
public abstract record WorkItem([property: JsonConverter(typeof(JsonStringEnumConverter))] WorkType Type)
{
    public record MSBuildProject(string ProjectPath)
        : WorkItem(WorkType.MSBuildProject);

    public record MSBuildSolution(string SolutionPath, string[] ProjectPaths)
        : WorkItem(WorkType.MSBuildSolution);

    public record UnityPackage([property: JsonConverter(typeof(JsonStringEnumConverter))] PackageType PackageType, string PackageName, string ManifestPath)
        : WorkItem(WorkType.UnityPackage);

    public record BuckBuildTarget(string BuildTarget)
        : WorkItem(WorkType.BuckBuildTarget);
}
