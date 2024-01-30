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

using GleanLocation = Glean.Indexer.Schema.CSharp.Location;
using GleanType = Glean.Indexer.Schema.CSharp.Type;

namespace Glean.Indexer;

public class GleanSyntaxWalker : CSharpSyntaxWalker
{
    public FactStore FactStore { get; init; }
    private SemanticModel Model { get; init; }

    public GleanSyntaxWalker(FactStore factStore, SemanticModel model)
    {
        Model = model;
        FactStore = factStore;
    }

    public void VisitDeclaration(SyntaxNode node)
    {
        var symbol = Model.GetDeclaredSymbol(node);

        if (symbol is not null &&
            DefinitionLocationFact.TryFromSymbol(symbol, out var definitionLocation) &&
            definitionLocation is not null)
        {
            FactStore.AddRange(definitionLocation);
        }
    }

    public override void VisitNamespaceDeclaration(NamespaceDeclarationSyntax node)
    {
        base.VisitNamespaceDeclaration(node);
    }

    public override void VisitClassDeclaration(ClassDeclarationSyntax node)
    {
        base.VisitClassDeclaration(node);
        VisitDeclaration(node);
    }

    public override void VisitStructDeclaration(StructDeclarationSyntax node)
    {
        base.VisitStructDeclaration(node);
        VisitDeclaration(node);
    }

    public override void VisitInterfaceDeclaration(InterfaceDeclarationSyntax node)
    {
        base.VisitInterfaceDeclaration(node);
        VisitDeclaration(node);
    }

    public override void VisitRecordDeclaration(RecordDeclarationSyntax node)
    {
        base.VisitRecordDeclaration(node);
        VisitDeclaration(node);
    }

    public override void VisitMethodDeclaration(MethodDeclarationSyntax node)
    {
        base.VisitMethodDeclaration(node);
        VisitDeclaration(node);
    }

    public override void VisitFieldDeclaration (FieldDeclarationSyntax node)
    {
        base.VisitFieldDeclaration(node);

        foreach (var variable in node.Declaration.Variables)
        {
            VisitDeclaration(variable);
        }
    }

    public override void VisitPropertyDeclaration(PropertyDeclarationSyntax node)
    {
        base.VisitPropertyDeclaration(node);
        VisitDeclaration(node);
    }

    public override void VisitLocalDeclarationStatement(LocalDeclarationStatementSyntax node)
    {
        base.VisitLocalDeclarationStatement(node);

        foreach (var variable in node.Declaration.Variables)
        {
            VisitDeclaration(variable);
        }
    }

    public override void VisitObjectCreationExpression(ObjectCreationExpressionSyntax node)
    {
        base.VisitObjectCreationExpression(node);

        var methodSymbol = Model.GetSymbolInfo(node).Symbol as IMethodSymbol;
        var typeSymbol = Model.GetSymbolInfo(node.Type).Symbol as ITypeSymbol;

        if (methodSymbol is not null &&
            MethodFact.TryFromSymbol(methodSymbol, out var method) &&
            method is not null &&
            GleanLocation.TryFromLocation(node.GetLocation(), out var location) &&
            location is not null &&
            typeSymbol is not null &&
            GleanType.TryFromSymbol(typeSymbol, out var type) &&
            type is not null)
        {
            var key = new ObjectCreationLocationFactKey(type, method,location);
            FactStore.Add(new ObjectCreationLocationFact(key));
        }
    }

    public override void VisitInvocationExpression(InvocationExpressionSyntax node)
    {
        base.VisitInvocationExpression(node);

        var methodSymbol = Model.GetSymbolInfo(node).Symbol as IMethodSymbol;

        if (methodSymbol is not null &&
            MethodFact.TryFromSymbol(methodSymbol, out var method) &&
            method is not null &&
            GleanLocation.TryFromLocation(node.GetLocation(), out var location) &&
            location is not null)
        {
            var key = new MethodInvocationLocationFactKey(method,location);
            FactStore.Add(new MethodInvocationLocationFact(key));
        }
    }
}

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
