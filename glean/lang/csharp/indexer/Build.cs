// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.MSBuild;
using Microsoft.Build.Locator;
using System;
using System.Collections.Generic;
using System.IO;
using Serilog;

namespace Indexer;

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

        Log.Debug($"DOTNET_ROOT={dotnetRoot}");
        Log.Debug($"MSBUILD_EXE_PATH={msbuildExePath}");
        Log.Information($"Using MSBuild from {msbuildPath}");

        MSBuildLocator.RegisterMSBuildPath(msbuildPath);
    }

    public static Compilation? CompileProject(MSBuildWorkspace workspace, string projectPath)
    {
        var project = workspace.OpenProjectAsync(projectPath).Result;
        return project.GetCompilationAsync().Result;
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
