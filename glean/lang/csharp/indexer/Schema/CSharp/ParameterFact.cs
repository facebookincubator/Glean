// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

using System.Linq;
using Microsoft.CodeAnalysis;

namespace Indexer.Schema.CSharp;

public record struct ParameterFactKey
    ( NameFact Name
    , Type Type
    , RefKind RefKind
    , bool IsThis
    , bool IsParams
    , bool IsOptional
    );

public record ParameterFact(ParameterFactKey Key) : FactWithKey<ParameterFactKey>(Predicate.Method, Key)
{
    public static bool TryFromSymbol(IParameterSymbol symbol, out ParameterFact? result)
    {
        var name = new NameFact(symbol.Name);
        if (Type.TryFromSymbol(symbol.Type, out var type) && type != null)
        {
            var key = new ParameterFactKey
                ( name
                , type
                , symbol.RefKind
                , symbol.IsThis
                , symbol.IsParams
                , symbol.IsOptional
                );

            result = new ParameterFact(key);
            return true;
        }

        result = default;
        return false;
    }
}
