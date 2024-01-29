/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

using Glean.Discovery;
using Glean.Indexer.Schema;
using System.Text.Json;
using Serilog;
using System;
using System.Collections.Generic;
using System.CommandLine;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using Newtonsoft.Json.Linq;

namespace Glean.Indexer;

public class Program
{
    const string LoggerTemplate = "[{Timestamp:yyyy-MM-ddTHH:mm:ss.fff}] [{Level:u4}] {Message:lj}{NewLine}{Exception}";

    public static void Main(string[] args)
    {
        var workPathArgument = new Argument<string>(
            name: "materialized-work-path",
            description: "The path of a file containing work for the indexer, encoded as a JSON array"
        );

        var outOption = new Option<string>(
            aliases: new [] { "--out", "-o" },
            description: "The path of a directory into which the indexer will write facts",
            getDefaultValue: () => "indexer_output"
        );

        var rootCommand = new RootCommand("Index C# projects")
        {
            workPathArgument,
            outOption
        };

        rootCommand.SetHandler
        (
            (workPath, outputPath) =>
            {
                Log.Logger = new LoggerConfiguration()
                    .MinimumLevel
                    .Verbose()
                    .WriteTo
                    .Console(
                        outputTemplate: LoggerTemplate,
                        restrictedToMinimumLevel: Serilog.Events.LogEventLevel.Information)
                    .CreateLogger();

                Log.Debug($"Repository root: {Hg.RepoRoot}");
                Directory.SetCurrentDirectory(Hg.RepoRoot);

                Log.Debug($"Creating output directory: {outputPath}");
                Directory.CreateDirectory(outputPath);

                Log.Debug($"Reading work from: {workPath}");
                var work = JsonSerializer.Deserialize<MaterializedWorkItem[]>(File.ReadAllText(workPath));

                if (work == null || !work.Any())
                {
                    Log.Warning("Nothing to do: work file was empty");
                    return;
                }

                Log.Debug("Initializing MSBuild");
                Build.Initialize();

                foreach (var workItem in work)
                {
                    IndexWorkItem(workItem, outputPath);
                }
            },
            workPathArgument,
            outOption
        );

        rootCommand.Invoke(args);
    }

    private static void IndexWorkItem(MaterializedWorkItem workItem, string outputPath)
    {
        switch (workItem)
        {
            case MaterializedWorkItem.MSBuildProject msbuildProjectWorkItem:
            {
                var projectPath = msbuildProjectWorkItem.ProjectPath;
                Log.Information($"Indexing MSBuild project {projectPath}");
                Indexer.IndexProject(projectPath, outputPath);
                break;
            }
            case MaterializedWorkItem.MSBuildSolution msbuildSolutionWorkItem:
            {
                Log.Information($"Indexing MSBuild solution {msbuildSolutionWorkItem.SolutionPath}");
                foreach (var projectPath in msbuildSolutionWorkItem.ProjectPaths)
                {
                    Indexer.IndexProject(projectPath, outputPath);
                }

                break;
            }
            case MaterializedWorkItem.UnityPackage unityPackageWorkItem:
            {
                var projectPath = unityPackageWorkItem.GeneratedProjectPath;
                Log.Information($"Indexing generated project {projectPath} from package {unityPackageWorkItem.PackageName}");
                Indexer.IndexProject(projectPath, outputPath);
                break;
            }
            default:
            {
                Log.Error($"Unsupported work type: {workItem.Type}");
                break;
            }
        }
    }
}
