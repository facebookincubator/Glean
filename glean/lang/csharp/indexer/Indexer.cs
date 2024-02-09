/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

using Glean.Discovery;
using Glean.Indexer.Schema;
using Glean.Indexer.Schema.CSharp;
using Glean.Indexer.Schema.Src;
using Microsoft.Build.Locator;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.MSBuild;
using Newtonsoft.Json.Linq;
using Serilog;
using Serilog.Events;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Diagnostics.Contracts;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.Json;

namespace Glean.Indexer;

public class Indexer
{
    public static void IndexWorkItem(FactStore factStore, MaterializedWorkItem workItem, string outputPath, LogEventLevel logLevel)
    {
        Log.Information($"Discovered work-item:\n{JsonSerializer.Serialize(workItem, new JsonSerializerOptions { WriteIndented = true })}");

        switch (workItem)
        {
            case MaterializedWorkItem.MSBuildProject msbuildProjectWorkItem:
            {
                var projectPath = msbuildProjectWorkItem.ProjectPath;

                var projectFact = ProjectFact.MSBuild(projectPath);
                factStore.Add(projectFact);

                BuildAndIndexProject(factStore, projectPath, outputPath, logLevel);
                break;
            }
            case MaterializedWorkItem.MSBuildSolution msbuildSolutionWorkItem:
            {
                var solutionFact = new SolutionFact(Hg.GetRepoRootRelativePath(msbuildSolutionWorkItem.SolutionPath));

                foreach (var projectPath in msbuildSolutionWorkItem.ProjectPaths)
                {
                    var projectFact = ProjectFact.MSBuild(projectPath);
                    var solutionToProjectFactKey = new SolutionToProjectFactKey
                        ( solutionFact
                        , projectFact
                        );
                    factStore.Add(new SolutionToProjectFact(solutionToProjectFactKey));

                    BuildAndIndexProject(factStore, projectPath, outputPath, logLevel);
                }

                break;
            }
            case MaterializedWorkItem.UnityPackage unityPackageWorkItem:
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
                factStore.Add(new ProjectFact(new ProjectFactKey(projectSource)));

                BuildAndIndexProject(factStore, projectPath, outputPath, logLevel);
                break;
            }
            default:
            {
                Log.Error($"Unsupported work type: {workItem.Type}");
                break;
            }
        }
    }

    public static void BuildAndIndexProject(FactStore factStore, string projectPath, string outputPath, LogEventLevel logLevel)
    {
        try
        {
            using (var workspace = MSBuildWorkspace.Create())
            {
                var relativeProjectPath = Hg.GetRepoRootRelativePath(projectPath);
                Log.Information($"Started building project: {relativeProjectPath}");

                var stopwatch = Stopwatch.StartNew();
                var compilation = Build.CompileProject(workspace, projectPath);

                stopwatch.Stop();
                Log.Information($"Finished building in {FormatElapsedTime(stopwatch)}");
                stopwatch.Reset();

                LogDiagnostics(compilation, relativeProjectPath);

                Log.Information($"Started indexing project: {relativeProjectPath}");
                var factCountBefore = factStore.CurrentFactCount;
                stopwatch.Start();
                foreach (var syntaxTree in compilation.SyntaxTrees)
                {
                    try
                    {
                        var visitor = new GleanSyntaxWalker(
                            factStore,
                            compilation.GetSemanticModel(syntaxTree)
                        );
                        visitor.Visit(syntaxTree.GetRoot());
                    }
                    catch (Exception e)
                    {
                        Log.Error(e.Message);
                    }
                }
                stopwatch.Stop();
                var factCountAfter = factStore.CurrentFactCount;
                Log.Information($"Finished indexing in {FormatElapsedTime(stopwatch)}");
                Log.Information($"Added {factCountAfter - factCountBefore} facts");
            }
        }
        catch (Exception e)
        {
            Log.Error(e.Message);
        }
    }

    private static string FormatElapsedTime(Stopwatch stopwatch)
    {
        TimeSpan elapsed = stopwatch.Elapsed;
        return string.Format
        (
            "{0:00}:{1:00}:{2:00}.{3:000}",
            elapsed.Hours,
            elapsed.Minutes,
            elapsed.Seconds,
            elapsed.Milliseconds
        );
    }

    private static void LogDiagnostics(Compilation compilation, string relativeProjectPath)
    {
        var diagnostics = compilation.GetDiagnostics();
        if (diagnostics.Where(d => d.Severity >= DiagnosticSeverity.Warning).Any())
        {
            Log.Warning($"Encountered one or more errors while building {relativeProjectPath}");
        }

        foreach (var diagnostic in diagnostics)
        {
            var code = diagnostic.Id;
            var error = diagnostic.GetMessage();
            var location = diagnostic.Location.GetLineSpan();
            var message = $"[{code}] {error}\n  at {location}";

            Log.Debug(message);
        }
    }
}
