// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

using Microsoft.CodeAnalysis;
using Serilog;
using System.Linq;

namespace Indexer.Schema.CSharp;

public record struct RecordFactKey
    ( FullNameFact Name
    , RecordFact? BaseType
    , TypeParameterFact[] TypeParameters
    , NamedType? ContainingType
    , Accessibility DeclaredAccessibility
    , bool IsAbstract
    , bool IsSealed
    );


public record RecordFact(RecordFactKey Key) : FactWithKey<RecordFactKey>(Predicate.Interface, Key)
{
    public static bool TryFromSymbol(INamedTypeSymbol symbol, out RecordFact? result)
    {
        if (FullNameFact.TryFromSymbol(symbol, out var name) && name != null)
        {
            RecordFact? baseType = default;
            if (symbol.BaseType != null)
            {
                if (!RecordFact.TryFromSymbol(symbol.BaseType, out baseType))
                {
                    result = default;
                    return false;
                }
            }

             TypeParameterFact[] typeParameters = symbol
                .TypeParameters
                .Select(typeParameterSymbol => TypeParameterFact.FromSymbol(typeParameterSymbol))
                .ToArray();

            NamedType? containingType = default;
            if (symbol.ContainingType != null)
            {
                if (!NamedType.TryFromSymbol(symbol.ContainingType, out containingType))
                {
                    result = default;
                    return false;
                }
            }

            var key = new RecordFactKey
                ( name
                , baseType
                , typeParameters
                , containingType
                , symbol.DeclaredAccessibility
                , symbol.IsAbstract
                , symbol.IsSealed
                );

            result = new RecordFact(key);
            return true;
        }

        result = default;
        return false;
    }
}
