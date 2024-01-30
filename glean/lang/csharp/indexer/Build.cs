/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.MSBuild;
using Microsoft.Build.Locator;
using System;
using System.Collections.Generic;
using System.IO;
using Serilog;

namespace Glean.Indexer;

public static class Build
{
    const string REPO_RELATIVE_DOTNET_ROOT = "arvr/projects/socialvr/third-party/dotnet/linux-x64";
    const string SDK_VERSION = "6.0.408";

    public static void Initialize()
    {
        var dotnetRoot = Path.Combine(Hg.RepoRoot, REPO_RELATIVE_DOTNET_ROOT);
        var msbuildPath = Path.Combine(dotnetRoot, "sdk", SDK_VERSION);
        var msbuildExePath = Path.Combine(msbuildPath, "MSBuild.dll");

        Environment.SetEnvironmentVariable("DOTNET_ROOT", dotnetRoot, EnvironmentVariableTarget.Process);
        Environment.SetEnvironmentVariable("MSBUILD_EXE_PATH", msbuildExePath, EnvironmentVariableTarget.Process);

        Log.Information($"DOTNET_ROOT={Hg.GetRepoRootRelativePath(dotnetRoot)}");
        Log.Information($"MSBUILD_EXE_PATH={Hg.GetRepoRootRelativePath(msbuildExePath)}");
        Log.Information($"Using MSBuild from {Hg.GetRepoRootRelativePath(msbuildPath)}");

        MSBuildLocator.RegisterMSBuildPath(msbuildPath);
    }

    public static Compilation CompileProject(MSBuildWorkspace workspace, string projectPath)
    {
        var project = workspace.OpenProjectAsync(projectPath).Result;
        var compilation = project.GetCompilationAsync().Result;

        if (compilation == null)
        {
            throw new Exception($"Failed to build project {projectPath}");
        }

        return compilation;
    }

    public static IEnumerable<ISymbol> EnumerateSymbols(INamespaceOrTypeSymbol symbol)
    {
        foreach (var member in symbol.GetMembers())
        {
            yield return member;

            if (member is INamespaceOrTypeSymbol namespaceOrTypeSymbol)
            {
                foreach (var nestedSymbol in EnumerateSymbols(namespaceOrTypeSymbol))
                {
                    yield return nestedSymbol;
                }
            }
        }
    }
}
