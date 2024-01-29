// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

using System;
using System.Threading.Tasks;
using System.CommandLine;
using System.IO;
using Newtonsoft.Json;
using Newtonsoft.Json.Linq;
using System.Collections.Generic;
using System.Linq;
using Serilog;

namespace Indexer;

public class Program
{
    const string LoggerTemplate = "[{Timestamp:yyyy-MM-ddTHH:mm:ss.fff}] [{Level:u4}] {Message:lj}{NewLine}{Exception}";

    public static async Task Main(string[] args)
    {
        var projectsArgument = new Argument<string>(
            name: "projects",
            description: "The path of a file containing a list of project paths (.csproj) to be indexed."
        );

        var outOption = new Option<string>(
            aliases: new [] { "--out", "-o" },
            description: "The path of a directory into which the indexer will write facts",
            getDefaultValue: () => "indexer_output"
        );

        var factsLimitOption = new Option<int>(
            aliases: new [] { "--factslimit", "-l" },
            description: "The maximum number of facts collected before writing to file. If used, several files with different index will be created.",
            getDefaultValue: () => 0
        );

        var rootCommand = new RootCommand("Index C# projects")
        {
            projectsArgument,
            outOption,
            factsLimitOption
        };

        rootCommand.SetHandler
        (
            async (projectsFile, @out, @factsLimit) =>
            {
                var logfile = Path.Join(@out, "debug.log");

                Log.Logger = new LoggerConfiguration()
                    .MinimumLevel.Verbose()
                    .WriteTo.Console(
                        outputTemplate: LoggerTemplate,
                        restrictedToMinimumLevel: Serilog.Events.LogEventLevel.Information)
                    .WriteTo.File(
                        logfile,
                        outputTemplate: LoggerTemplate,
                        restrictedToMinimumLevel: Serilog.Events.LogEventLevel.Verbose)
                    .CreateLogger();

                Log.Debug($"Repository root: {Hg.RepoRoot}");
                Directory.SetCurrentDirectory(Hg.RepoRoot);

                Log.Debug($"Creating output directory: {@out}");
                Directory.CreateDirectory(@out);

                if (@factsLimit > 0)
                {
                    Log.Information($"Facts limit: {@factsLimit}");
                }
                else
                {
                    Log.Information($"No facts limit specified. All facts for a project will be saved in a single file.");
                }

                Log.Debug($"Reading projects from: {projectsFile}");
                var projectPaths = JsonConvert.DeserializeObject<List<string>>
                (
                    File.ReadAllText(projectsFile)
                );

                if (projectPaths == null || !projectPaths.Any())
                {
                    Log.Warning("No work to do: projects file was empty");
                    return;
                }

                Log.Debug("Initializing MSBuild");
                Build.Initialize();

                foreach (var projectPath in projectPaths)
                {
                    try
                    {
                        Indexer.IndexProject(projectPath, @out, @factsLimit);
                    }
                    catch (Exception ex)
                    {
                        Log.Error(ex.Message);
                    }
                }

                Log.Information($"See {logfile} for full logs");
            },
            projectsArgument,
            outOption,
            factsLimitOption
        );

        await rootCommand.InvokeAsync(args);
    }
}
