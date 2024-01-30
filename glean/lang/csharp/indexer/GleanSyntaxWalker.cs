/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

using Glean.Indexer.Schema;
using Glean.Indexer.Schema.CSharp;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

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
