/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

using Facebook.SocialVR.Packages;

namespace Glean.Discovery;

public abstract record WorkItem(WorkType Type)
{
    public record MSBuildProject(string ProjectPath)
        : WorkItem(WorkType.MSBuildProject);

    public record MSBuildSolution(string SolutionPath, string[] ProjectPaths)
        : WorkItem(WorkType.MSBuildSolution);

    public record UnityPackage(PackageType PackageType, string PackageName, string ManifestPath)
        : WorkItem(WorkType.UnityPackage);

    public record BuckBuildTarget(string BuildTarget)
        : WorkItem(WorkType.BuckBuildTarget);
}
