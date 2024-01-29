// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

using System;
using Microsoft.CodeAnalysis.MSBuild;
using System.Diagnostics;
using System.Diagnostics.Contracts;
using System.Linq;
using System.Collections.Generic;
using Indexer.Schema;
using Indexer.Schema.CSharp;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Newtonsoft.Json.Linq;
using Microsoft.Build.Locator;
using Serilog;
using System.Text;

using GleanLocation = Indexer.Schema.CSharp.Location;
using GleanType = Indexer.Schema.CSharp.Type;

namespace Indexer;

public class GleanSyntaxWalker : CSharpSyntaxWalker
{
    private SemanticModel Model { get; }
    private FactFile FactFile { get; }

    public GleanSyntaxWalker(SemanticModel model, FactFile factFile)
    {
        FactFile = factFile;
        Model = model;
    }

    public void VisitDeclaration(SyntaxNode node)
    {
        var symbol = Model.GetDeclaredSymbol(node);

        if (symbol is not null &&
            DefinitionLocationFact.TryFromSymbol(symbol, out var definitionLocation) &&
            definitionLocation is not null)
        {
            FactFile.AddFacts(definitionLocation.ToList());
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
            FactFile.AddFact(new ObjectCreationLocationFact(key));
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
            FactFile.AddFact(new MethodInvocationLocationFact(key));
        }
    }
}

public class Indexer
{
    public static int IndexProject(string projectPath, string outputDirectory, int factsLimit)
    {
        int factsTotalCount = 0;
        Log.Information($"Indexing project: {projectPath}");

        using (var workspace = MSBuildWorkspace.Create())
        {
            Log.Debug($"Started building project: {projectPath}");

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

            Log.Debug($"Finished building in {formattedTime}");

            var diagnostics = compilation.GetDiagnostics();
            if (diagnostics.Where(d => d.Severity >=  DiagnosticSeverity.Warning).Any())
            {
                Log.Warning($"Encountered one or more errors while building {projectPath}");
            }

            foreach (var diagnostic in diagnostics)
            {
                var code = diagnostic.Id;
                var error = diagnostic.GetMessage();
                var location = diagnostic.Location.GetLineSpan();
                var message = $"[{code}] {error}\n  at {location}";

                switch (diagnostic.Severity)
                {
                    case DiagnosticSeverity.Error:
                    case DiagnosticSeverity.Warning:
                        Log.Warning(message);
                        break;
                    default:
                        Log.Debug(message);
                        break;
                }
            }

            var factFile = new FactFile(projectPath, outputDirectory, factsLimit);

            foreach (var syntaxTree in compilation.SyntaxTrees)
            {
                var visitor = new GleanSyntaxWalker(compilation.GetSemanticModel(syntaxTree), factFile);
                visitor.Visit(syntaxTree.GetRoot());
            }

            // Add the last facts
            factFile.CreateFile();
            factsTotalCount = factFile.FactsTotalCount;
        }

        Log.Information($"Created {factsTotalCount} facts");

        return factsTotalCount;
    }
}
