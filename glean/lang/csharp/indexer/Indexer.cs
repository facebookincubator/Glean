/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

using Glean.Indexer.Schema;
using Glean.Indexer.Schema.CSharp;
using Microsoft.Build.Locator;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.MSBuild;
using Newtonsoft.Json.Linq;
using Serilog;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Diagnostics.Contracts;
using System.IO;
using System.Linq;
using System.Text;

namespace Glean.Indexer;

public class Indexer
{
    public static void IndexProject(FactStore factStore, string projectPath, string outputPath)
    {
        try
        {
            var facts = new List<Fact>();

            using (var workspace = MSBuildWorkspace.Create())
            {
                Log.Information($"Started building project: {projectPath}");

                Stopwatch stopwatch = Stopwatch.StartNew();

                var compilation = Build.CompileProject(workspace, projectPath);
                if (compilation == null)
                {
                    throw new Exception($"Failed to build project {projectPath}");
                }

                stopwatch.Stop();
                TimeSpan elapsed = stopwatch.Elapsed;
                string formattedTime = string.Format
                (
                    "{0:00}:{1:00}:{2:00}",
                    elapsed.Hours,
                    elapsed.Minutes,
                    elapsed.Seconds
                );

                Log.Information($"Finished building in {formattedTime}");

                var diagnostics = compilation.GetDiagnostics();
                if (diagnostics.Where(d => d.Severity >= DiagnosticSeverity.Warning).Any())
                {
                    Log.Warning($"Encountered one or more errors while building {projectPath}");
                }

                foreach (var diagnostic in diagnostics)
                {
                    var code = diagnostic.Id;
                    var error = diagnostic.GetMessage();
                    var location = diagnostic.Location.GetLineSpan();
                    var message = $"[{code}] {error}\n  at {location}";

                    Log.Debug(message);
                }

                foreach (var syntaxTree in compilation.SyntaxTrees)
                {
                    var visitor = new GleanSyntaxWalker(
                        factStore,
                        compilation.GetSemanticModel(syntaxTree)
                    );
                    visitor.Visit(syntaxTree.GetRoot());
                }
            }
        }
        catch (Exception e)
        {
            Log.Error(e.Message);
        }
    }
}
