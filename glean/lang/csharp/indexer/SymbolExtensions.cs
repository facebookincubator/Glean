// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

using System.Text;
using Microsoft.CodeAnalysis;

namespace Indexer;

public static class SymbolExtensions
{
    public static bool IsGlobalNamespaceSymbol(this ISymbol symbol)
    {
        return symbol is INamespaceSymbol namespaceSymbol && namespaceSymbol.IsGlobalNamespace;
    }
}
