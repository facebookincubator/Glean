// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

using Microsoft.CodeAnalysis;
using Serilog;
using System.Linq;
using System.Collections.Generic;

namespace Indexer.Schema.CSharp;

public record struct LocalFactKey
    ( NameFact Name
    , Type Type
    , MethodFact? ContainingMethod
    , RefKind RefKind
    , bool isConst
    );

public record LocalFact(LocalFactKey Key) : FactWithKey<LocalFactKey>(Predicate.Local, Key)
{
    public static bool TryFromSymbol(ILocalSymbol symbol, out LocalFact? result)
    {
        var name = new NameFact(symbol.Name);

        if (!Type.TryFromSymbol(symbol.Type, out var type))
        {
            result = default;
            return false;
        }

        IMethodSymbol methodSymbol = symbol.ContainingSymbol as IMethodSymbol;
        MethodFact? containingMethod = null;
        if (methodSymbol != null)
        {
            MethodFact.TryFromSymbol(methodSymbol, out containingMethod);
        }

        if (containingMethod == null)
        {
            // This is not allowed in schema
            result = default;
            return false;

        }

        var key = new LocalFactKey
            ( name
            , type
            , containingMethod
            , symbol.RefKind
            , symbol.IsConst
            );

        result = new LocalFact(key);
        return true;
    }
}
