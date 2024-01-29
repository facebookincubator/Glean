// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

using Microsoft.CodeAnalysis;
using Serilog;
using System.Linq;

namespace Indexer.Schema.CSharp;

public record struct InterfaceFactKey
    ( FullNameFact Name
    , TypeParameterFact[] TypeParameters
    , NamedType? ContainingType
    , Accessibility DeclaredAccessibility
    , bool IsStatic
    );

public record InterfaceFact(InterfaceFactKey Key) : FactWithKey<InterfaceFactKey>(Predicate.Interface, Key)
{
    public static bool TryFromSymbol(INamedTypeSymbol symbol, out InterfaceFact? result)
    {
        if (FullNameFact.TryFromSymbol(symbol, out var name) && name != null)
        {
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

            var key = new InterfaceFactKey
                ( name
                , typeParameters
                , containingType
                , symbol.DeclaredAccessibility
                , symbol.IsStatic
                );

            result = new InterfaceFact(key);
            return true;
        }

        result = default;
        return false;
    }
}
