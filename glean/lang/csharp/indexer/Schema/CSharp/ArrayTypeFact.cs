// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

using System.Linq;
using Newtonsoft.Json.Linq;
using Microsoft.CodeAnalysis;

namespace Indexer.Schema.CSharp;

public record struct ArrayTypeFactKey
    ( Type ElementType
    , byte Rank
    );

public record ArrayTypeFact(ArrayTypeFactKey Key) : FactWithKey<ArrayTypeFactKey>(Predicate.ArrayType, Key)
{
    public static bool TryFromSymbol(IArrayTypeSymbol symbol, out ArrayTypeFact? result)
    {
        if (Type.TryFromSymbol(symbol.ElementType, out var elementType) && elementType != null)
        {
            var key = new ArrayTypeFactKey
                ( elementType
                , (byte)symbol.Rank
                );

            result = new ArrayTypeFact(key);
            return true;
        }

        result = default;
        return false;
    }
}
