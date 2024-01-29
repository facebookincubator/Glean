/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

using Facebook.SocialVR.Packages;

namespace Glean.Discovery;

public abstract record MaterializedWorkItem(WorkType Type)
{
    public record MSBuildProject(string ProjectPath)
        : MaterializedWorkItem(WorkType.MSBuildProject);

    public record MSBuildSolution(string SolutionPath, string[] ProjectPaths)
        : MaterializedWorkItem(WorkType.MSBuildSolution);

    public record UnityPackage(string GeneratedProjectPath, PackageType PackageType, string PackageName)
        : MaterializedWorkItem(WorkType.UnityPackage);

    public record BuckBuildTarget(string GeneratedProjectPath, string BuildTarget)
        : MaterializedWorkItem(WorkType.BuckBuildTarget);
}
