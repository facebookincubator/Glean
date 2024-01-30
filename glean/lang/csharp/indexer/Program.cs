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
using Serilog.Events;
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

        var validLogLevels = string.Join(", ", Enum.GetNames(typeof(LogEventLevel)));

        var logLevelOption = new Option<LogEventLevel>(
            "--log-level",
            isDefault: true,
            description: $"Sets the minimum log-level. Log events of lower severity will be discarded",
            parseArgument: result =>
            {
                if (!result.Tokens.Any())
                {
                    return LogEventLevel.Information;
                }

                if (Enum.TryParse<LogEventLevel>(result.Tokens.Single().Value, ignoreCase: true, out LogEventLevel logLevel))
                {
                    return logLevel;
                }

                result.ErrorMessage = $"Expected one of [{validLogLevels}]";
                return LogEventLevel.Information;
            }
        );

        var rootCommand = new RootCommand("Index C# projects")
        {
            workPathArgument,
            outOption,
            logLevelOption,
        };

        rootCommand.SetHandler
        (
            (workPath, outputPath, logLevel) =>
            {
                Log.Logger = new LoggerConfiguration()
                    .MinimumLevel
                    .Verbose()
                    .WriteTo
                    .Console(
                        outputTemplate: LoggerTemplate,
                        restrictedToMinimumLevel: logLevel)
                    .CreateLogger();

                Log.Information($"Repository root: {Hg.RepoRoot}");
                Directory.SetCurrentDirectory(Hg.RepoRoot);

                Log.Information($"Creating output directory: {outputPath}");
                Directory.CreateDirectory(outputPath);

                Log.Information($"Reading work from: {workPath}");
                var work = JsonSerializer.Deserialize<MaterializedWorkItem[]>(File.ReadAllText(workPath));

                if (work == null || !work.Any())
                {
                    Log.Warning("Nothing to do: work file was empty");
                    return;
                }

                Log.Information("Initializing MSBuild");
                Build.Initialize();

                var factStore = new FactStore(
                    capacity: 1000,
                    outputPath
                );

                foreach (var workItem in work)
                {
                    IndexWorkItem(factStore, workItem, outputPath);
                }

                factStore.Flush();
            },
            workPathArgument,
            outOption,
            logLevelOption
        );

        rootCommand.Invoke(args);
    }

    private static void IndexWorkItem(FactStore factStore, MaterializedWorkItem workItem, string outputPath)
    {
        switch (workItem)
        {
            case MaterializedWorkItem.MSBuildProject msbuildProjectWorkItem:
            {
                var projectPath = msbuildProjectWorkItem.ProjectPath;
                Log.Information($"Indexing MSBuild project {projectPath}");
                Indexer.IndexProject(factStore, projectPath, outputPath);
                break;
            }
            case MaterializedWorkItem.MSBuildSolution msbuildSolutionWorkItem:
            {
                Log.Information($"Indexing MSBuild solution {msbuildSolutionWorkItem.SolutionPath}");
                foreach (var projectPath in msbuildSolutionWorkItem.ProjectPaths)
                {
                    Indexer.IndexProject(factStore, projectPath, outputPath);
                }

                break;
            }
            case MaterializedWorkItem.UnityPackage unityPackageWorkItem:
            {
                var projectPath = unityPackageWorkItem.GeneratedProjectPath;
                Log.Information($"Indexing generated project {projectPath} from package {unityPackageWorkItem.PackageName}");
                Indexer.IndexProject(factStore, projectPath, outputPath);
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
