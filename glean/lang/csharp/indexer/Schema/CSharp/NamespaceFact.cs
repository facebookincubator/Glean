// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

using Microsoft.CodeAnalysis;

namespace Indexer.Schema.CSharp;

public record struct NamespaceFactKey(NameFact Name, NamespaceFact? ContainingNamespace);

public record NamespaceFact(NamespaceFactKey Key) : FactWithKey<NamespaceFactKey>(Predicate.Namespace, Key)
{
    public static NamespaceFact GlobalNamespace()
    {
        var name = NameFact.Empty();
        NamespaceFact? containingNamespace = default;
        var key = new NamespaceFactKey(name, containingNamespace);

        return new NamespaceFact(key);
    }

    public static NamespaceFact FromSymbol(INamespaceSymbol symbol)
    {
        if (symbol.IsGlobalNamespace)
        {
            return GlobalNamespace();
        }

        var name = new NameFact(symbol.Name);
        var containingNamespace = FromSymbol(symbol.ContainingNamespace);
        var key = new NamespaceFactKey(name, containingNamespace);

        return new NamespaceFact(key);
    }
}
