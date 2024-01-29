/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

using Glean.Discovery;
using Serilog;
using Serilog.Sinks.SystemConsole.Themes;
using System;
using System.CommandLine;

var repoRoot = new Argument<string>(
    name: "repo-root",
    description: "The absolute path of the current repository root"
);

var workOutputPath = new Argument<string>(
    name: "work-output-path",
    description: "Discovered work will be written to this path, encoded as a JSON array"
);

var discoverMSBuild = new Command(name: "msbuild")
{
    repoRoot,
    workOutputPath,
};

discoverMSBuild.SetHandler(Discovery.DiscoverMSBuild, repoRoot, workOutputPath);

var discoverUnity = new Command(name: "unity")
{
    repoRoot,
    workOutputPath
};

discoverUnity.SetHandler(Discovery.DiscoverUnity, repoRoot, workOutputPath);

var discover = new Command(name: "discover")
{
    discoverMSBuild,
    discoverUnity
};

var unityProjectTemplatePath = new Argument<string>(
    name: "unity-project-template-path",
    description: "The path of a template used to generate projects for Unity packages"
);

var materializedWorkOutputPath = new Argument<string>(
    name: "materialized-work-output-path",
    description: "Materialized work will be written to this path, encoded as a JSON array"
);

var materialize = new Command(name: "materialize")
{
    unityProjectTemplatePath,
    materializedWorkOutputPath
};

materialize.SetHandler(Discovery.Materialize, unityProjectTemplatePath, materializedWorkOutputPath);

var rootCommand = new RootCommand(description: "Prepare work for the Glean C# indexer")
{
    discover,
    materialize
};

Log.Logger = new LoggerConfiguration()
    .MinimumLevel.Verbose()
    .WriteTo
    .Console(
        outputTemplate: "[{Timestamp:yyyy-MM-ddTHH:mm:ss.fff}] [{Level:u4}] {Message:lj}{NewLine}{Exception}",
        restrictedToMinimumLevel: Serilog.Events.LogEventLevel.Information,
        theme: AnsiConsoleTheme.Code
    )
    .CreateLogger();

return rootCommand.Invoke(args);
