/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

using Microsoft.CodeAnalysis;
using Serilog;

namespace Glean.Indexer.Schema.CSharp;

public record struct FullNameFactKey(NameFact Name, NamespaceFact ContainingNamespace);

public record FullNameFact(FullNameFactKey Key) : FactWithKey<FullNameFactKey>(Predicate.FullName, Key)
{
    public static bool TryFromSymbol(ISymbol symbol, out FullNameFact? result)
    {
        var name = new NameFact(symbol.Name);
        var containingNamespaceSymbol = symbol.ContainingNamespace;

        if (containingNamespaceSymbol == null)
        {
            Log.Error($"{symbol} does not have a containing namespace");
            result = default;
            return false;
        }

        var containingNamespace = NamespaceFact.FromSymbol(containingNamespaceSymbol);
        var key = new FullNameFactKey(name, containingNamespace);

        result = new FullNameFact(key);
        return true;
    }
}
