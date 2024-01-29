/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

using Microsoft.CodeAnalysis;

namespace Glean.Indexer.Schema.CSharp;

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
