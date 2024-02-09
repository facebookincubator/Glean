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

        var indexProjectFilesOption = new Option<bool>(
            name: "--index-project-files",
            description: "Include build metadata in the index",
            getDefaultValue: () => true
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
            indexProjectFilesOption
        };

        rootCommand.SetHandler
        (
            (workPath, outputPath, logLevel, indexProjectFiles) =>
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

                if (!indexProjectFiles)
                {
                    Log.Warning($"Indexing of project files is disabled");
                }

                Log.Information("Initializing MSBuild");
                Build.Initialize();

                var factStore = new FactStore(
                    capacity: 1000,
                    outputPath
                );

                foreach (var workItem in work)
                {
                    Indexer.IndexWorkItem(factStore, workItem, outputPath, logLevel, indexProjectFiles);
                }

                factStore.Flush();
            },
            workPathArgument,
            outOption,
            logLevelOption,
            indexProjectFilesOption
        );

        rootCommand.Invoke(args);
    }
}
